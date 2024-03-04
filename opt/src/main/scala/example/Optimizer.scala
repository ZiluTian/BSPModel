package BSPModel

import scala.collection.mutable.{Map => MutMap, ArrayBuffer}
import scala.reflect.runtime.universe._
       
import sourcecode._

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

                            def combineMessages(ms: List[Message]): Option[Message] = b.combineMessages(ms)
                            def updateState(s: State, m: Option[Message]): State = b.updateState(s, m)
                            def stateToMessage(s: State): Message = b.stateToMessage(s)

                            override def run(ms: List[Message]): Unit = {
                                // println(f"Run is called! local ids are ${localIds} values are ${localIds.map(i => part.getMemberMessage[Message](i)).toList}")
                                state = run(state,  localIds.map(i => part.getMemberMessage[Message](i)).toList ::: ms)
                                // println(f"Updated state is ${state}")
                            }
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
                val bsps = part.topo.nodes.map(_._2)
                val bspType = bsps.head.getClass()
                // only vectorize BSPs that are created from the same non-type-parameterized class definition. Assume that bsps generated from the same class def SHARE THE SAME RUN METHOD
                
                if (bsps.forall(i => i.getClass == bspType)) {

                    val newArray = java.lang.reflect.Array.newInstance(bspType, bsps.size)
                    bsps.zipWithIndex.foreach(i => {
                        java.lang.reflect.Array.set(newArray, i._2, i._1)
                    })
                }

                Util.debug(bspType)
                Util.debug(bsps.head.state.getClass)

                // // If all BSPs have the same type, then vectorize it
                // if (bsps.tail.forall(i => i.state.getClass == stateType)){
                //     // create an array that contains only the State 
                //     val newArray = java.lang.reflect.Array.newInstance(stateType, bsps.size).asInstanceOf[Array[_]]
                //     bsps.zipWithIndex.foreach(i => {
                //         newArray.update(i._2, i._1.state)
                //     })
                //     // assert(newArray.asInstanceOf[Array[stateType]].size == bsps.size)
                //     val updateStateMethod = typeOf[BSP with ComputeMethod with DoubleBuffer].decl(TermName("run")).asMethod
                //     bsps.tail.forall { obj =>
                //         val objUpdateStateMethod = typeOf[obj.type].decl(TermName("run")).asMethod
                //         objUpdateStateMethod == updateStateMethod
                //     }
                //     Util.debug("all nodes have the same type")
                // } else {
                //     part
                // }
            } else {
                part
            }
            part
            // require(bsps.topo.nodes.map(_._2).forall(x => x.asInstanceOf[BSP with DoubleBuffer with ComputeMethod].State))
        }
    }
}