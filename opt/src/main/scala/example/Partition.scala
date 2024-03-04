// package BSPModel


// // https://stackoverflow.com/questions/10373318/mixing-in-a-trait-dynamically
// // A Partition tracks local BSPs within and edges that connecting with other partitions
// // local BSPs within are unordered
// // A collection of key, value pairs
// trait Partition extends Member {

//     type NodeId

//     val id: PartitionId

//     object Graph {
//         val nodes: Map[NodeId, Member]
//         val edges: Map[NodeId, List[NodeId]]
//         val cuts: Map[PartitionId, List[NodeId]]
//     }

//     def getMemberValue(k: NodeId): Member = {
//         Graph.nodes(k)
//     }

//     // Keys: adjacent partition id
//     // Values: external predecessors / successors in neighboring partitions
//     // val extPred: Map[PartitionId, Seq[NodeId]]
//     // external successors

//     // Keys: external valueIndex that have an internal predecessors
//     // Values: value indices in the local partition
    
//     // Keys: internal valueIndex that have an external successors
//     // Values: value indices in an external partition
//     // Built dynamically after receiving info of outEdges from neighboring partitions
//     // val inEdges: MutMap[NodeId, ArrayBuffer[NodeId]] = MutMap[NodeId, ArrayBuffer[NodeId]]()
// }

// class Partition[M, Id] extends Partition {
//     type NodeId = Id
//     type Member = M

//     def getMemberValue[K, V](k: K): V
// }

// object Partition {
//     val BSPPartition = new Partition[BSP, BSPId]{
//         def getMemberValue[BSPId, BSP](k: BSPId): BSP = {
//             members.toMap.get(k)
//         }
//     }
// }
// // def compile(): BSP = {
// //         new BSP {
// //             val compute = new ComputeMethod {
// //                 // state: ((indexedLocalValue, outEdges), Option[PartitionMessage])
// //                 type State = ((Map[self.NodeId, self.Member], Seq[(self.NodeId, Seq[self.NodeId])]), Option[PartitionMessage{type M = self.Member; type Idx = self.NodeId}])
// //                 type Message = PartitionMessage{type M = self.Member; type Idx = self.NodeId}

// //                 def combineMessages(m1: Message, m2: Message): Message = {
// //                     // require(m1.M =:= m2.M =:= self.Member)
// //                     // require(m1.Idx =:= m2.Idx =:= self.NodeId)

// //                     new PartitionMessage {
// //                         type M = self.Member
// //                         type Idx = self.NodeId

// //                         val value = m1.value ++ m2.value
// //                         val messageEncoding = m1.messageEncoding ++ m2.messageEncoding
// //                         val schema = m1.schema ++ m2.schema
// //                     }
// //                 }

// //                 def updateState(s: State, m: Option[Message]): State = {
// //                     (s._2, m) match {
// //                         case (_, None) =>
// //                             s
// //                         case (None, _) => 
// //                             (s._1, m)
// //                         case (Some(k), Some(l)) =>
// //                             (s._1, Some(combineMessages(k, l)))
// //                     }
// //                 }

// //                 def stateToMessage(s: State): Message = {
// //                     new PartitionMessage {
// //                         type M = self.Member
// //                         type Idx = self.NodeId

// //                         val messageEncoding = s._1._2.map(_._1).toSeq
// //                         val value = messageEncoding.map(i => s._1._1(i))

// //                         val schema = s._1._2.toMap
// //                     }
// //                 }
// //             }

// //             val state = ((self.indexedLocalValue, self.outEdges.toSeq), None)

// //             // Fixed communication
// //             val sendTo = FixedCommunication(extSucc.keys.map(k => partitionIdToBSPId(k)).toSeq)
// //         }
// //     }

// // trait IndexedPartition extends Partition {
// //     type Value
// //     type Member = (NodeId, Value)
// // }