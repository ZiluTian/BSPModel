package BSPModel

import scala.collection.mutable.ArrayBuffer

// Box for rewriting. Unbox during optimization
trait OutNeighbors extends Iterable[BSPId]

case class FixedCommunication(xs: Seq[BSPId]) extends OutNeighbors {
    def iterator = xs.iterator
}

case class DynamicCommunication(xs: ArrayBuffer[BSPId]) extends OutNeighbors {
    def iterator = xs.iterator
}

case class HybridCommunication(xs: FixedCommunication, ys: DynamicCommunication) extends OutNeighbors {
    def iterator = xs.iterator ++ ys.iterator
}