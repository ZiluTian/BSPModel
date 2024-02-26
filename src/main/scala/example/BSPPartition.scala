package BSPModel

// todo: express compile here incrementally from that of Partition, by compiling staged expr to overwrite updateState
abstract class BSPPartition extends Partition {self =>
    val id: PartitionId = getNextId()
    type Value = BSP
    type ValueIndex = BSPId

    override def compile(): BSP = {
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
 
                // todo: exec each BSP when updating state
                def updateState(s: State, m: Option[Message]): State = {
                    (s._2, m) match {
                        case (_, None) =>
                            ((s._1._1.map(j => (j._1, j._2.exec)), s._1._2), None)
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

            val state = ((self.indexedLocalValue, self.outEdges.toSeq), None)

            this.outEdges ++= extSucc.keys.map(k => partitionIdToBSPId(k))
        }
    }
}