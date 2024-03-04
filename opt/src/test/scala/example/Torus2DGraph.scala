package BSPModel
package test

trait TestGraph {
    val g: Map[Long, Iterable[Long]]
}

object TestGraph{
    implicit def toGraph(g: TestGraph): Map[Long, Iterable[Long]] = g.g
}

case class Torus2DGraph(width: Int, height: Int, startingIndex: Int = 0) extends TestGraph {
val g: Map[Long, IndexedSeq[Long]] = {
    Range(0, width * height).map(index => {
        val x = index % width
        val y = index / width

        val neighbors = for {
            i <- -1 to 1
            j <- -1 to 1
            if !(i == 0 && j == 0)
                dx = (x + i + width) % width
                dy = (y + j + height) % height
        } yield dy * width + dx
        (index.toLong + startingIndex, neighbors.map(n => n.toLong + startingIndex))
    }).toMap
    }
}