package BSPModel

import scala.collection.mutable.{Map => MutMap, ArrayBuffer}
import scala.reflect.runtime.universe._
       
import sourcecode._

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

// import BSPModelMacros.FunctionWrapper

// Optimizations apply directly to the Partition data structure, 
// like mutating the connectivity of local BSPs
// Computations are staged to be reified when lifting a HBSP to a BSP 
trait Optimizer[T <: Partition, V <: Partition] {
    def transform(part: T): V
}

object Optimizer {
    val bspToDoubleBuffer = new Optimizer[Partition{type Value=BSP; type NodeId = BSPId}, Partition{type Value=BSP with DoubleBuffer; type NodeId = BSPId}] {
        def transform(part: Partition{type Value = BSP; type NodeId = BSPId}): Partition{type Value = BSP with DoubleBuffer; type NodeId = BSPId} = {
            new Partition {
                import part.{topo=>tp}
                
                type NodeId = part.NodeId
                type Value = BSP with DoubleBuffer
                
                val id = part.id
                val topo: Graph[NodeId, Value] = Graph(tp.nodes.mapValues(b => DoubleBuffer.fromBSP(b.asInstanceOf[BSP with ComputeMethod])), tp.edges, tp.cuts)

                def getMemberMessage[V](k: NodeId): V = part.getMemberMessage[V](k)
            }
        }
    }

    // transform send to read
    val pushToPullAndUnbox = new Optimizer[Partition{type Value = BSP with DoubleBuffer; type NodeId = BSPId}, Partition{type Value = BSP with DoubleBuffer; type NodeId = BSPId}] {
        def transform(part: Partition{type Value = BSP with DoubleBuffer; type NodeId = BSPId}): Partition{type Value = BSP with DoubleBuffer; type NodeId = BSPId} = {           
            type Value = BSP with DoubleBuffer
            
            val id = part.id

            // preprocess, populate the metavariable readFrom
            val readFrom: MutMap[BSPId, ArrayBuffer[BSPId]] = MutMap[BSPId, ArrayBuffer[BSPId]]()

            part.topo.nodes.foreach(i => {
                val (bspId, bsp) = i
                bsp.sendTo match {
                    case FixedCommunication(xs) => {
                        xs.foreach(i => {
                            readFrom.getOrElseUpdate(i, new ArrayBuffer[BSPId]) += bspId
                        })
                        
                    }
                    case HybridCommunication(xs, ys) => {
                        xs.foreach(i => {
                            readFrom.getOrElseUpdate(i, new ArrayBuffer[BSPId]) += bspId
                        })
                    }
                    case _ =>
                }
            }) 

            // readFrom.foreach(i => println(f"$i,${i._2.mkString(", ")}"))
            // updatedNodes.foreach(i => println(f"${i._2.sendTo}"))
             

            new Partition {                
                type NodeId = BSPId
                type Value = BSP with DoubleBuffer

                val id = part.id
                val topo: Graph[NodeId, Value] = Graph(
                    part.topo.nodes.map(bsp => {
                        val (bid, b: BSP with ComputeMethod with DoubleBuffer) = bsp 
                        
                        def genNewBSP(localIds: Iterable[BSPId]): BSP with ComputeMethod with DoubleBuffer = new BSP with ComputeMethod with DoubleBuffer {

                            type State = b.State
                            type Message = b.Message
                            
                            var state = b.state
                            val id = b.id

                            val sendTo = b.sendTo match {
                                case FixedCommunication(_) => List()
                                case DynamicCommunication(xs) => xs
                                case HybridCommunication(xs, ys) => ys
                                case _ => b.sendTo 
                            }

                            stagedExpr = if (localIds.isEmpty) {
                                None
                            } else {
                                Some((localIds, (xs: Iterable[BSPId]) => combineMessages(xs.map(i => part.getMemberMessage[Message](i)).toList)))
                            }

                            def combineMessages(ms: List[Message]): Option[Message] = b.combineMessages(ms)
                            def updateState(s: State, m: Option[Message]): State = b.updateState(s, m)
                            def stateToMessage(s: State): Message = b.stateToMessage(s)

                            override def run(s: State, ms: List[Message]): State = {
                                stagedExpr match {
                                    case None => updateState(s, combineMessages(ms))
                                    case Some(x) => updateState(s, combineMessages(x._2(x._1).get :: ms))
                                }                              
                            }

                            // override def run(ms: List[Message]): Unit = {
                            //     // println(f"Run is called! local ids are ${localIds} values are ${localIds.map(i => part.getMemberMessage[Message](i)).toList}")
                            //     state = run(state,  localIds.map(i => part.getMemberMessage[Message](i)).toList ::: ms)
                            //     // println(f"Updated state is ${state}")
                            // }
                        }

                        (bid, readFrom.get(bid) match {
                            case None => b
                            case Some(xs) => genNewBSP(xs)
                        })
                    }).toMap, part.topo.edges, part.topo.cuts)

                def getMemberMessage[V](k: NodeId): V = part.getMemberMessage[V](k)
            }
        }
    }

    // target the current Pregel architecture, where all vertices share the same compute
    val vectorizeBSP = new Optimizer[Partition{type Value = BSP with DoubleBuffer; type NodeId = BSPId}, Partition{type Value = BSP with DoubleBuffer; type NodeId = BSPId}] {
       
        
        def transform(part: Partition{type Value = BSP with DoubleBuffer; type NodeId = BSPId}): Partition{type Value = BSP with DoubleBuffer; type NodeId = BSPId} = {
            if (part.topo.nodes.size > 1) {
                // get the runtime class of the BSP.State to check if all bsps have the same State type
                val (bspIds, bsps) = part.topo.nodes.toList.unzip
                val bspType = bsps.head.getClass()

                // only vectorize BSPs that are created from the same non-type-parameterized class definition. Assume that bsps generated from the same class def SHARE THE SAME RUN METHOD
                
                if (bsps.forall(i => i.getClass == bspType)) {
                    new Partition {
                        type NodeId = BSPId
                        type Value = BSP with DoubleBuffer

                        val id = part.id
                        
                        private val bsp = bsps.head.asInstanceOf[BSP with ComputeMethod with DoubleBuffer]
                        private val bspValueType = bsp.state.getClass()

                        val mergedBSP = new BSP with ComputeMethod with DoubleBuffer {
                            type State = Array[_]
                            
                            val id = part.id
                            val sendTo = List()

                            // todo: what to put for Value (M)
                            type Message = PartitionMessage{type M = Member; type Idx = NodeId}

                            var state = java.lang.reflect.Array.newInstance(bspValueType, bsps.size).asInstanceOf[State]

                            // todo: does not conform to PartitionMessage
                            var publicState = state.map(i => bsp.stateToMessage(i.asInstanceOf[bsp.State]))

                            def updateState(s: State, m: Option[Message]): State = ???
                            def stateToMessage(s: State): Message = ???
                            def combineMessages(ms: List[Message]): Option[Message] = ??? 
                            // {
                                // ms match {
                                //     case Nil => None
                                //     case m :: Nil => Some(m)
                                //     case m :: tail =>  Some(new PartitionMessage {
                                //         type M = Member
                                //         type Idx = NodeId

                                //         val value = tail.fold(m.value)((x, y) => x.value ++ y.value)
                                //         val messageEncoding = m1.messageEncoding ++ m2.messageEncoding
                                //         val schema = m1.schema ++ m2.schema
                                //     })
                                // }
                            // }

                            val stagedExprs = bsps.zipWithIndex.map(i => {
                                java.lang.reflect.Array.set(state, i._2, i._1.state)
                                i._1.stagedExpr match {
                                    case Some((localIds, exp)) => // transform localIds into array-based offset
                                        Some((localIds.map(i => bspIds.indexOf(i).toLong), exp))
                                    case _ => None
                                }
                            })

                            override def run(s: State, ms: List[Message]): State = {
                                s.zipWithIndex.map(i => {
                                    bsp.stagedExpr = stagedExprs(i._2).asInstanceOf[Option[(Iterable[BSPId], (Iterable[BSPId]) => Option[bsp.Message])]] 
                                    bsp.run(i._1.asInstanceOf[bsp.State], List())
                                    // println(f"The value of i after update is ${j}")
                                }).asInstanceOf[State]
                            }
                        }

                        val topo: Graph[NodeId, Value] = Graph(
                            // merged node has the same id as the partition id
                            Map(id -> mergedBSP),
                            part.topo.edges,
                            part.topo.cuts
                        )

                        def getMemberMessage[V](k: NodeId): V = topo.nodes.head._2.publicState.asInstanceOf[Array[_]](k.toInt).asInstanceOf[V]
                    }
                } else {
                    part
                }
            } else {
                part
            }
            // require(bsps.topo.nodes.map(_._2).forall(x => x.asInstanceOf[BSP with DoubleBuffer with ComputeMethod].State))
        }
    }
}