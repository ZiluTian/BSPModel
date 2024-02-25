package BSPModel

import scala.collection.LinearSeq

trait HierarchicalBSP {
    val partitions: LinearSeq[Partition]

    val optimizations: LinearSeq[Optimization]

    def cut(): LinearSeq[HierarchicalBSP] = ???

    def optimize(): HierarchicalBSP = {
        val updatedPartitions: LinearSeq[Partition] = optimizations.foldLeft(partitions)((parts: LinearSeq[Partition], opt: Optimization) => 
            parts.map(p => {
                opt.transform(p)
            })
        )

        new HierarchicalBSP {
            val partitions = updatedPartitions
            val optimizations = List()
        }
    }

    def compile(): BSP = ???
}

object HierarchicalBSP {
    def toBSP(x: HierarchicalBSP): BSP = {
        ???
    }
}