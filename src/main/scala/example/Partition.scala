package BSPModel

import scala.collection.mutable.{Map => MutMap, ArrayBuffer}

// https://stackoverflow.com/questions/10373318/mixing-in-a-trait-dynamically
// A Partition tracks local BSPs within and edges that connecting with other partitions
// local BSPs within are unordered
// A collection of key, value pairs
abstract class Partition {
    val id: PartitionId
    type Value
    type ValueIndex

    // Local BSP cores in the partition
    // Can be either linear seq or indexed seq
    val indexedLocalValue: Map[ValueIndex, Value]

    // Keys: adjacent partition id
    // Values: external predecessors / successors in neighboring partitions
    val extPred: Map[PartitionId, Seq[ValueIndex]]
    val extSucc: Map[PartitionId, Seq[ValueIndex]]

    // Keys: external valueIndex that have an internal predecessors
    // Values: value indices in the local partition
    val outEdges: MutMap[ValueIndex, ArrayBuffer[ValueIndex]] = MutMap[ValueIndex, ArrayBuffer[ValueIndex]]()
    
    // Keys: internal valueIndex that have an external successors
    // Values: value indices in an external partition
    // Built dynamically after receiving info of outEdges from neighboring partitions
    val inEdges: MutMap[ValueIndex, ArrayBuffer[ValueIndex]] = MutMap[ValueIndex, ArrayBuffer[ValueIndex]]()

    def preprocess(): Unit

    def compile(): BSP = ???
}

abstract class BSPPartition extends Partition {
    val id: PartitionId = Partition.getNextId()
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

    // def compile(): BSP = { self: BSPPartition => 
    //     val compute: ComputeMethod = ComputeMethod {
    //         type State = Seq[self.State]
    //         type Message = PartitionMessage

    //         def combineMessage(m1: Message, m2: Message): Message = {
                
    //         }
            
    //     }
    // }
}

object Partition {
    private var lastId: PartitionId = 0

    def getNextId(): PartitionId = this.synchronized {
        lastId = lastId + 1
        lastId
    } 
}