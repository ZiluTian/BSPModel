package BSPModel

import scala.collection.mutable.{Map => MutMap, ArrayBuffer}

trait Optimizer[T <: Partition, V <: Partition] extends DebugTrace{
    def transform(part: T): V
}

object Optimize {
    def default(part: Partition{type Member = BSP & ComputeMethod; type NodeId = BSPId}): Partition{type Member = BSP & ComputeMethod; type NodeId = BSPId} = {
        StageAndFuseLocalCommunication.transform(
            StageRemoteCommunication.transform(
                RefineCommunication.transform(part)
            )
        )
    }
}

// (BSP, (fixedLocal, fixedRemote))
case object RefineCommunication extends Optimizer[
    Partition{type Member = BSP & ComputeMethod; type NodeId = BSPId}, 
    Partition{type Member = (BSP & ComputeMethod, (Iterable[BSPId], Iterable[BSPId])); type NodeId = BSPId}] {
    
    def transform(part: Partition{type Member = BSP & ComputeMethod; type NodeId = BSPId}): Partition{type Member = (BSP & ComputeMethod, (Iterable[BSPId], Iterable[BSPId])); type NodeId = BSPId} = {
        new Partition {self => 
            type NodeId = BSPId
            type Member = (BSP & ComputeMethod, (Iterable[BSPId], Iterable[BSPId]))

            val id = part.id
            val topo: Graph[NodeId] = part.topo
            val members: List[Member] = part.members.map(bsp => {
                (bsp, bsp.receiveFrom match {
                    case FixedCommunication(xs) => {
                        xs.partition(i => part.topo.vertices.contains(i))
                    }
                    case HybridCommunication(xs, ys) => {
                        xs.partition(i => part.topo.vertices.contains(i))
                    }
                    case _ => (List(), List())
                })
            })
        }
    }
}

// Cascaded staging
// stage 1: compile away remote messages with cache
// stage 2: compile away local messages and fuse vertices
case object StageRemoteCommunication extends Optimizer[
    Partition{type Member = (BSP & ComputeMethod, (Iterable[BSPId], Iterable[BSPId])); type NodeId = BSPId}, 
    Partition{type Member = (BSP & ComputeMethod & Stage, (Iterable[BSPId], Iterable[BSPId])); type NodeId = BSPId}] {
    
    def transform(part: Partition{type Member = (BSP & ComputeMethod, (Iterable[BSPId], Iterable[BSPId])); type NodeId = BSPId}): Partition{type Member = (BSP & ComputeMethod & Stage, (Iterable[BSPId], Iterable[BSPId])); type NodeId = BSPId} = {

        new Partition {
            type NodeId = BSPId
            type Member = (BSP & ComputeMethod & Stage, (Iterable[BSPId], Iterable[BSPId]))

            val id = part.id
            // pad with inCache
            val topo = ArrayGraph.fromGraph(part.topo)
            val members: List[Member] = part.members.map(bspIt => {
                val bsp = bspIt._1
                val remoteIds = bspIt._2._2

                ddbg(f"Local ids for ${bsp.id} are ${bspIt._2._1.mkString(", ")} remote ids are ${remoteIds.mkString(", ")}")

                def genNewBSP(): BSP & ComputeMethod & Stage = 
                    new BSP with ComputeMethod with Stage { selfBSP => 

                        type State = bsp.State
                        type Message = bsp.Message
                        
                        var state = bsp.state
                        val id = bsp.id
                        val receiveFrom = bsp.receiveFrom

                        val stagedComputation: List[StagedExpr] = 
                            if (remoteIds.size > 0) {
                                List(new StagedExpr {
                                    type Message = selfBSP.Message

                                    private val receiveRemote: Iterable[(PartitionId, Int)] = remoteIds.map(i => topo.asInstanceOf[ArrayGraph[BSPId]].getInboxCacheIndex(i).get)

                                    override def compile(): Option[Message] = {
                                        selfBSP.partialCompute(receiveRemote.map(i => {
                                            topo.asInstanceOf[ArrayGraph[BSPId]].inCache(i._1)(i._2).asInstanceOf[bsp.Message]
                                        }))
                                    }
                                    ddbg("Receive from contains remote values " + receiveRemote)
                                })
                            } else {
                                List()
                            }

                        def partialCompute(ms: Iterable[Message]): Option[Message] = bsp.partialCompute(ms)
                        def updateState(s: State, m: Option[Message]): State = bsp.updateState(s, m)
                        def stateToMessage(s: State): Message = bsp.stateToMessage(s)
                }
                (genNewBSP(), (bspIt._2._1, remoteIds))
            })
        }
    }
}

// Fuse multiple BSPs into one BSP whose state is Array[BSP & ComputeMethod & DoubleBuffer]
// Double buffering is needed to ensure correct semantics
// Lift messages up from vertex-level to partition-level
case object StageAndFuseLocalCommunication extends Optimizer[
    Partition{type Member = (BSP & ComputeMethod & Stage, (Iterable[BSPId], Iterable[BSPId])); type NodeId = BSPId}, 
    Partition{type Member = BSP & ComputeMethod; type NodeId = BSPId}]{

        def transform(part: Partition{type Member = (BSP & ComputeMethod & Stage, (Iterable[BSPId], Iterable[BSPId])); type NodeId = BSPId}): Partition{type Member = BSP & ComputeMethod; type NodeId = BSPId} = {
            val bspIds = part.members.map(bsp => bsp._1.asInstanceOf[BSP].id)
            val localIds = part.members.flatMap(bsp => bsp._2._1)

            ddbg("Local ids are " + localIds.mkString(", "))

            new Partition {self =>
                type NodeId = BSPId
                type Member = BSP & ComputeMethod

                val id = part.id
                val topo: Graph[NodeId] = part.topo

                def genNewBSP(bsp: BSP & ComputeMethod & Stage, receiveFromLocal: Iterable[BSPId]): BSP & ComputeMethod & DoubleBuffer & Stage = 
                    new BSP with ComputeMethod with Stage with DoubleBuffer { selfBSP => 

                        type State = bsp.State
                        type Message = bsp.Message
                        
                        var state = bsp.state
                        var publicState = bsp.stateToMessage(bsp.state)
                        val id = bsp.id
                        val receiveFrom = bsp.receiveFrom

                        val stagedComputation: List[StagedExpr] = 
                            if (localIds.size > 0) {
                                new StagedExpr {
                                    type NodeId = Int
                                    type Message = selfBSP.Message

                                    private val receiveLocal: Iterable[Int] = receiveFromLocal.map(i => bspIds.indexOf(i))

                                    ddbg(selfBSP.id + " receive local values are " + receiveLocal.mkString(" ,"))

                                    override def compile(): Option[Message] = {
                                        assert(receiveLocal.size > 0)
                                        
                                        selfBSP.partialCompute(receiveLocal.map(i => members.head.state.asInstanceOf[(Array[BSP & ComputeMethod & Stage & DoubleBuffer], Option[PartitionMessage{type M = BSP; type Idx = NodeId}])]._1(i).publicState.asInstanceOf[Message]))
                                    }
                                } :: bsp.stagedComputation
                            } else {
                                bsp.stagedComputation
                            }

                        def partialCompute(ms: Iterable[Message]): Option[Message] = bsp.partialCompute(ms)
                        def updateState(s: State, m: Option[Message]): State = bsp.updateState(s, m)
                        def stateToMessage(s: State): Message = bsp.stateToMessage(s)
                }

                // merged BSP no longer has a publicState for other BSPs
                val mergedBSP = new BSP with ComputeMethod {
                    // padded with cached message results
                    type State = (Array[BSP & ComputeMethod & Stage & DoubleBuffer], Option[PartitionMessage{type M = BSP; type Idx = NodeId}])
                    
                    val id = part.id
                    // partition ids
                    // This restricts fuse operation to each partition
                    // Cannot fuse different parts of a partition separately
                    val receiveFrom = part.topo.inEdges.keySet

                    type Message = PartitionMessage{type M = BSP; type Idx = NodeId}

                    var state: State = (part.members.map(b => genNewBSP(b._1.asInstanceOf[BSP & ComputeMethod & Stage], b._2._1)).toArray.asInstanceOf[Array[BSP & ComputeMethod & Stage & DoubleBuffer]], None)

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
                                    val schema = s._2.get.schema ++ m2.schema
                                }                                        
                                (s._1, Some(combinedMsg))
                            }
                        }
                    }

                    def stateToMessage(s: State): Message = ???
                    def partialCompute(ms: Iterable[Message]): Option[Message] = {
                        ms match {
                            case Nil => None
                            case m :: Nil => Some(m)
                            case _ => 
                                Some(new PartitionMessage {
                                    type M = BSP
                                    type Idx = NodeId

                                    val value = ms.map(i => i.value).flatten.toList
                                    val messageEncoding = ms.map(i => i.messageEncoding).flatten.toList
                                    val schema = ms.flatMap(i => i.schema).toMap
                                })
                        }
                    }

                    // in-place update to each BSP inside
                    override def run(ms: Iterable[Message]): Unit = {
                        state._1.foreach(bsp => {
                            bsp.run(List())
                        })
                        state._1.foreach(_.updatePublicState())
                    }

                    override def toString(): String = {
                        state._1.map(_.state).mkString(", ")
                    }
                }

                val members = List(mergedBSP)
            }
        }
}