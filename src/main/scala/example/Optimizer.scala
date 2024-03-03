package BSPModel

import scala.collection.mutable.{Map => MutMap, ArrayBuffer}

// Optimizations apply directly to the Partition data structure, 
// like mutating the connectivity of local BSPs
// Computations are staged to be reified when lifting a HBSP to a BSP 
trait Optimizer[T <: Partition, V <: Partition] {
    def transform(bsps: T): V
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
}