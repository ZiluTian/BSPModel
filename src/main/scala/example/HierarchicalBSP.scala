package BSPModel

// Hierarchical BSP is a scope whose members are partitions
// optionally with some optimizations
trait HierarchicalBSP extends Scope with Partition {self =>
    type M = Partition

    // trait Optimization {
    //     def transform(bsps: Partition): Partition
    // }

    // val optimizations: List[Optimization]

    // // def cut(): List[HierarchicalBSP] = ???

    // def optimize(): HierarchicalBSP = {
    //     val updatedPartitions = optimizations.foldLeft(members)((parts: List[Partition], opt: Optimization) => 
    //         parts.map(p => {
    //             opt.transform(p)
    //         })
    //     )

    //     new HierarchicalBSP {
    //         val members = updatedPartitions
    //         val optimizations = List()
    //     }
    // }
}