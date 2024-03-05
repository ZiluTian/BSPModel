package BSPModel

import scala.collection.mutable.{Map => MutMap, ArrayBuffer}
import scala.reflect.runtime.universe._
       
import sourcecode._

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

// Optimizations apply directly to the Partition data structure, 
// like mutating the connectivity of local BSPs
// Computations are staged to be reified when lifting a HBSP to a BSP 
trait Optimizer[T <: Partition, V <: Partition] {
    def transform(part: T): V
}

object Optimizer {
    val bspToDoubleBuffer = new Optimizer[Partition{type Member=BSP with ComputeMethod; type NodeId = BSPId}, Partition{type Member=BSP with ComputeMethod with DoubleBuffer; type NodeId = BSPId}] {
        def transform(part: Partition{type Member = BSP with ComputeMethod; type NodeId = BSPId}): Partition{type Member = BSP with ComputeMethod with DoubleBuffer; type NodeId = BSPId} = {
            new Partition {
                                
                type NodeId = part.NodeId
                type Member = BSP with ComputeMethod with DoubleBuffer
                
                val id = part.id
                val members = part.members.map(b => DoubleBuffer.fromBSP(b))

                val topo: Graph[NodeId] = part.topo
            }
        }
    }

    // transform send to read
    val pushToPullAndUnbox = new Optimizer[Partition{type Member = BSP with ComputeMethod with DoubleBuffer; type NodeId = BSPId}, Partition{type Member = BSP with ComputeMethod with DoubleBuffer; type NodeId = BSPId}] {
        def transform(part: Partition{type Member = BSP with ComputeMethod with DoubleBuffer; type NodeId = BSPId}): Partition{type Member = BSP with ComputeMethod with DoubleBuffer; type NodeId = BSPId} = {           
            type Member = BSP with ComputeMethod with DoubleBuffer
            
            val id = part.id

            // preprocess, populate the metavariable readFrom
            val readFrom: MutMap[BSPId, ArrayBuffer[BSPId]] = MutMap[BSPId, ArrayBuffer[BSPId]]()

            part.members.foreach(bsp => {
                bsp.sendTo match {
                    case FixedCommunication(xs) => {
                        xs.foreach(i => {
                            readFrom.getOrElseUpdate(i, new ArrayBuffer[BSPId]) += bsp.id
                        })
                        
                    }
                    case HybridCommunication(xs, ys) => {
                        xs.foreach(i => {
                            readFrom.getOrElseUpdate(i, new ArrayBuffer[BSPId]) += bsp.id
                        })
                    }
                    case _ =>
                }
            }) 

            // readFrom.foreach(i => println(f"$i,${i._2.mkString(", ")}"))
            // updatedNodes.foreach(i => println(f"${i._2.sendTo}"))

            val indexedId: Map[Long, Int] = part.members.zipWithIndex.map(i => (i._1.id, i._2)).toMap

            new Partition {                
                type NodeId = BSPId
                type Member = BSP with ComputeMethod with DoubleBuffer

                val id = part.id
                val members: List[Member] = part.members.map(bsp => {
                    def genNewBSP(localIds: Iterable[BSPId]): BSP with ComputeMethod with DoubleBuffer = 
                        new BSP with ComputeMethod with DoubleBuffer { selfBSP => 

                            type State = bsp.State
                            type Message = bsp.Message
                            
                            var state = bsp.state
                            val id = bsp.id

                            val sendTo = bsp.sendTo match {
                                case FixedCommunication(_) => List()
                                case DynamicCommunication(xs) => xs
                                case HybridCommunication(xs, ys) => ys
                                case _ => bsp.sendTo 
                            }

                            val stagedExpr: Option[StagedExpr] = Some(new StagedExpr {
                                type NodeId = Int
                                type Message = selfBSP.Message

                                val receiveFrom: List[Int] = localIds.toList.map(i => indexedId(i))
                                override def compile(): Message = {
                                    assert(receiveFrom.size > 0)
                                    println("Receive from contains values " + receiveFrom)
                                    selfBSP.combineMessages(receiveFrom.map(i => {
                                        members(i).asInstanceOf[BSP with DoubleBuffer].publicState.asInstanceOf[Message]
                                    })).get
                                }
                            })

                            def combineMessages(ms: List[Message]): Option[Message] = bsp.combineMessages(ms)
                            def updateState(s: State, m: Option[Message]): State = bsp.updateState(s, m)
                            def stateToMessage(s: State): Message = bsp.stateToMessage(s)
                    }

                    readFrom.get(bsp.id) match {
                        case Some(xs) if xs.size > 0 => genNewBSP(xs)
                        case _ => bsp
                    }
                })

                val topo: Graph[NodeId] = part.topo
            }
        }
    }

    // hierarchical, each cell is still a BSP
    // rely on the getMemberMessage defined in the partition to access local values in the Array state. The partition can contain only one such a nested BSP
    // compile away staged expr in the resulting BSP (no longer DoubleBuffer)
    val mergeBSP = new Optimizer[Partition{type Member = BSP with ComputeMethod with DoubleBuffer; type NodeId = BSPId}, Partition{type Member = BSP with ComputeMethod; type NodeId = BSPId}] {
        
        def transform(part: Partition{type Member = BSP with ComputeMethod with DoubleBuffer; type NodeId = BSPId}): Partition{type Member = BSP with ComputeMethod; type NodeId = BSPId} = {
            assert(part.members.size >= 1)
            
            // if (part.members.size > 1) {
                // get the runtime class of the BSP.State to check if all bsps have the same State type
            val bspIds = part.members.map(bsp => bsp.asInstanceOf[BSP].id)
                            
            new Partition {self =>
                type NodeId = BSPId
                type Member = BSP with ComputeMethod

                val id = part.id

                def genNewBSP(b: BSP with ComputeMethod with DoubleBuffer): BSP with ComputeMethod with DoubleBuffer = 
                    new BSP with ComputeMethod with DoubleBuffer { selfBSP => 
                        type State = b.State
                        type Message = b.Message
                        
                        var state = b.state
                        val id = b.id

                        val sendTo = b.sendTo

                        val stagedExpr: Option[StagedExpr] = if (b.stagedExpr.isEmpty) {
                            None
                        } else {
                            Some(new StagedExpr {
                                type NodeId = Int
                                type Message = selfBSP.Message
                                val receiveFrom: List[Int] = b.stagedExpr.get.receiveFrom.map(r => bspIds.indexOf(r))

                                override def compile(): Message = {
                                    selfBSP.combineMessages(receiveFrom.map(i => members.head.state.asInstanceOf[(Array[BSP with ComputeMethod with DoubleBuffer], Option[PartitionMessage{type M = BSP; type Idx = NodeId}])]._1(i).publicState.asInstanceOf[Message])).get
                                }
                            })
                        }

                        def combineMessages(ms: List[Message]): Option[Message] = b.combineMessages(ms)
                        def updateState(s: State, m: Option[Message]): State = b.updateState(s, m)
                        def stateToMessage(s: State): Message = b.stateToMessage(s)
                    }

                // merged BSP no longer has a publicState for other BSPs
                val mergedBSP = new BSP with ComputeMethod {
                    // padded with cached message results
                    // without preprocessing to know the 
                    type State = (Array[BSP with ComputeMethod with DoubleBuffer], Option[PartitionMessage{type M = BSP; type Idx = NodeId}])
                    
                    val id = part.id
                    val sendTo = List()

                    // todo: what to put for Member (M)
                    type Message = PartitionMessage{type M = BSP; type Idx = NodeId}

                    var state: State = (part.members.map(b => genNewBSP(b.asInstanceOf[BSP with ComputeMethod with DoubleBuffer])).toArray.asInstanceOf[Array[BSP with ComputeMethod with DoubleBuffer]], None)

                    // simply update the message component of the state
                    // executing BSPs in the array is done in in-place run
                    def updateState(s: State, m: Option[Message]): State = {
                        m match {
                            case None => s
                            case Some(m2) => if (s._2.isEmpty){
                                (s._1, Some(m2))
                            } else {
                                    val combinedMsg = new PartitionMessage {
                                        type M = BSP
                                        type Idx = NodeId

                                        val value = s._2.get.value ++ m2.value
                                        val messageEncoding = s._2.get.messageEncoding ++ m2.messageEncoding
                                        val schema = s._2.get.schema ++ m2.schema }                                        
                                (s._1, Some(combinedMsg))
                            }
                        }
                    }

                    def stateToMessage(s: State): Message = ???
                    def combineMessages(ms: List[Message]): Option[Message] = {
                        ms match {
                            case Nil => None
                            case m :: Nil => Some(m)
                            case _ => 
                                Some(new PartitionMessage {
                                    type M = BSP
                                    type Idx = NodeId

                                    val value = ms.map(i => i.value).flatten
                                    val messageEncoding = ms.map(i => i.messageEncoding).flatten
                                    val schema = ms.flatMap(i => i.schema).toMap
                                })
                        }
                    }

                    // in-place update to each BSP inside
                    override def run(ms: List[Message]): Unit = {
                        state._1.foreach(bsp => {
                            bsp.run(List())
                        })
                        state._1.foreach(_.updatePublicState)
                    }

                    override def toString(): String = {
                        state._1.map(_.state).mkString(", ")
                    }
                }

                val members = List(mergedBSP)
                val topo: Graph[NodeId] = part.topo
            }
        }
    }
}