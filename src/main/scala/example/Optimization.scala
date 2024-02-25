package BSPModel

// Optimizations apply directly to the Partition data structure, 
// like mutating the connectivity of local BSPs
// Computations are staged to be reified when lifting a HBSP to a BSP 
trait Optimization {
    def transform(bsps: Partition): Partition
}

// object Optimimzation {
//     // for a collection of
//     val readFromLocal: IndexedSeq[Iterable[BSPId]]
// }