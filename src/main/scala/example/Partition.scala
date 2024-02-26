package BSPModel

import scala.collection.mutable.{Map => MutMap, ArrayBuffer}

// https://stackoverflow.com/questions/10373318/mixing-in-a-trait-dynamically
// A Partition tracks local BSPs within and edges that connecting with other partitions
// local BSPs within are unordered
// A collection of key, value pairs
abstract class Partition {self: Partition => 
    val id: PartitionId
    type Value
    type ValueIndex

    // Local BSP cores in the partition
    // Can be either linear seq or indexed seq
    val indexedLocalValue: Map[ValueIndex, Value]

    // Keys: adjacent partition id
    // Values: external predecessors / successors in neighboring partitions
    // val extPred: Map[PartitionId, Seq[ValueIndex]]
    val extSucc: Map[PartitionId, Seq[ValueIndex]]

    // Keys: external valueIndex that have an internal predecessors
    // Values: value indices in the local partition
    val outEdges: MutMap[ValueIndex, ArrayBuffer[ValueIndex]] = MutMap[ValueIndex, ArrayBuffer[ValueIndex]]()
    
    // Keys: internal valueIndex that have an external successors
    // Values: value indices in an external partition
    // Built dynamically after receiving info of outEdges from neighboring partitions
    // val inEdges: MutMap[ValueIndex, ArrayBuffer[ValueIndex]] = MutMap[ValueIndex, ArrayBuffer[ValueIndex]]()

    def preprocess(): Unit

    // In the simplest partition (without computations or communications within local values),
    // compute simply appends received messages.
    def compile(): BSP = {
        new BSP {
            val compute = new ComputeMethod {
                // state: ((indexedLocalValue, outEdges), Option[PartitionMessage])
                type State = ((Map[self.ValueIndex, self.Value], Seq[(self.ValueIndex, Seq[self.ValueIndex])]), Option[PartitionMessage{type M = self.Value; type Idx = self.ValueIndex}])
                type Message = PartitionMessage{type M = self.Value; type Idx = self.ValueIndex}

                def combineMessages(m1: Message, m2: Message): Message = {
                    // require(m1.M =:= m2.M =:= self.Value)
                    // require(m1.Idx =:= m2.Idx =:= self.ValueIndex)

                    new PartitionMessage {
                        type M = self.Value
                        type Idx = self.ValueIndex

                        val value = m1.value ++ m2.value
                        val messageEncoding = m1.messageEncoding ++ m2.messageEncoding
                        val schema = m1.schema ++ m2.schema
                    }
                }

                def updateState(s: State, m: Option[Message]): State = {
                    (s._2, m) match {
                        case (_, None) =>
                            s
                        case (None, _) => 
                            (s._1, m)
                        case (Some(k), Some(l)) =>
                            (s._1, Some(combineMessages(k, l)))
                    }
                }

                def stateToMessage(s: State): Message = {
                    new PartitionMessage {
                        type M = self.Value
                        type Idx = self.ValueIndex

                        val messageEncoding = s._1._2.map(_._1).toSeq
                        val value = messageEncoding.map(i => s._1._1(i))

                        val schema = s._1._2.toMap
                    }
                }
            }

            val state = ((self.indexedLocalValue, self.outEdges.toSeq.map(i => (i._1, i._2.toSeq))), None)

            this.outEdges ++= extSucc.keys.map(k => partitionIdToBSPId(k))
        }
    }
}

abstract class BSPPartition extends Partition {
    val id: PartitionId = getNextId()
    type Value = BSP
    type ValueIndex = BSPId

    def preprocess(): Unit = {
        // Lift external edges of each bsp to outEdges of the partition
        indexedLocalValue.foreach(idBSP => {
            val extRefs = idBSP._2.outEdges.filter(x => !indexedLocalValue.contains(x))
            idBSP._2.outEdges --= extRefs
            outEdges.getOrElseUpdate(idBSP._1, new ArrayBuffer[BSPId]) ++= extRefs
        })
    }

    // def compile(): BSP = {
    //     // Functions to compare two types
    //     // def compareTypes[A, B](implicit ev: A=:=B): Boolean = true

    //     var visibleBoard: Array[Value] = indexedLocalValue.values.toArray
    //     var privateBoard: Array[Value] = visibleBoard.clone()

    //     val compute: ComputeMethod = new ComputeMethod {
    //         type State = Array[self.Value]
    //         type Message = PartitionMessage

    //         def combineMessage(m1: PartitionMessage, m2: PartitionMessage): PartitionMessage = {
    //             new PartitionMessage {
    //                 type M = Any
    //                 type Idx = Any

    //                 val value = m1.value ++ m2.value
    //                 val messageEncoding = m1.messageEncoding ++ m2.messageEncoding
    //                 val schema = m1.schema ++ m2.schema
    //             }
    //         }

    //         // If no external message, then only update locally
    //         def updateState(s: State, m: Option[Message]): State = {
    //             m match {
    //                 case None => 
    //                     s.map(i => self.compute.run(i, partialEval()))
                        
    //             }
    //         }
    //     }
    // }
}