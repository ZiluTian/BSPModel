package BSPModel

import scala.util.Random

// Game of life example
class GoLSpec extends munit.FunSuite {

  class Cell(alive: Boolean, pos: BSPId) extends BSP {
    val state: Boolean = alive
    id = pos

    val compute = new ComputeMethod {
      type State = Boolean
      type Message = Int

      def combineMessages(m1: Int, m2: Int): Int = {
          m1 + m2
      }

      def updateState(s: Boolean, m: Option[Int]): Boolean = {
          m match {
              case None => s
              case Some(x) =>                 
                if (s && (x > 3 || x < 2)) {
                    false
                } else {
                    true
                }
          }
      }

      def stateToMessage(s: Boolean): Int = {
          if (s) 1 else 0
      }
    }
  }

  trait Graph {
    val g: Map[Long, Iterable[Long]]
  }

  object Graph{
    implicit def toGraph(g: Graph): Map[Long, Iterable[Long]] = g.g
  }


  case class Torus2DGraph(width: Int, height: Int, startingIndex: Int = 0) extends Graph {
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

  test("say hello") {
    val width = 10
    val height = 10
    val g = new Torus2DGraph(width, height, 0)

    val initPartition = new BSPPartition {
      val indexedLocalValue: Map[BSPId, BSP] = {
        g.map(i => {
          val a = new Cell(Random.nextBoolean(), i._1)
          a.outEdges ++= i._2
          (i._1, a)
        })
      }
      // val extPred = Map[PartitionId, Seq[BSPId]]()
      val extSucc = Map[PartitionId, Seq[BSPId]]()

      val outEdges = indexedLocalValue.map(idBSP => {
            val extRefs = idBSP._2.outEdges.filter(x => !indexedLocalValue.contains(x))
            idBSP._2.outEdges --= extRefs
            (idBSP._1, extRefs.toSeq)
        }).groupBy(_._1).mapValues(_.flatMap(_._2).toSeq).toMap
    
      // println(outEdges)
    }

    val hbsp = new HierarchicalBSP {
      val partitions = List(initPartition)
      val optimizations = List(Localization)
    }

    // Localization.transform(initPartition)
    val optHBSP = hbsp.optimize()
    
    initPartition.indexedLocalValue.values.foreach(i => {
      println(i.ast)
    })
  }
}
