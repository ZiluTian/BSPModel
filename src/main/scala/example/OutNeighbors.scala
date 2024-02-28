package BSPModel

import scala.collection.mutable.ArrayBuffer

trait OutNeighbors {
    def traverse(): Iterable[BSPId]
}

case class FixedCommunication(xs: Seq[BSPId]) extends OutNeighbors {
    def traverse = xs.toIterable
}

case class DynamicCommunication(xs: ArrayBuffer[BSPId]) extends OutNeighbors {
    def traverse = xs.toIterable
}

case class HybridCommunication(xs: FixedCommunication, ys: DynamicCommunication) extends OutNeighbors {
    def traverse = {
        xs.traverse ++ ys.traverse
    }
}