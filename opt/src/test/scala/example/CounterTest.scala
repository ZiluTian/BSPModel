package BSPModel
package test

import scala.util.Random
import sourcecode._
import BSPModel.Util._

import org.scalatest._
import funsuite._

// A counter example that adds received messages to the current value
class CounterTest extends AnyFunSuite {
    trait CounterCompute extends ComputeMethod {
        type State = Int
        type Message = Int

        def partialCompute(m1: Iterable[Int]): Option[Int] = {
            if (m1.isEmpty) {
                None
            } else {
                Some(m1.sum)
            }
        }

        def updateState(s: Int, m: Option[Int]): Int = {
            m match {
                case None => s
                case Some(x) =>
                    s + x
            }
        }

        def stateToMessage(s: Int): Int = {
            s
        }
    }

    class Cell(pos: BSPId, neighbors: Seq[BSPId]) extends BSP with CounterCompute {
        var state: Int = 1
        override val id = pos
        val receiveFrom = FixedCommunication(neighbors) 
    } 

    test("Counter should increse its value in every round") {
        val width = 100
        val height = 100
        val g = new Torus2DGraph(width, height, 0)

        val agents = g.map(i => new Cell(i._1, i._2.toSeq))

        // binding information (partition structure)
        val initPartition = new Partition {
            type Member = BSP & ComputeMethod
            type NodeId = BSPId
            type Value = BSP
            val id = 1

            val topo = new BSPModel.Graph[BSPId]{
                val vertices = agents.map(a => a.id).toSet
                val edges = TestGraph.toGraph(g).map(i => (i._1, i._2.toList))
                val inEdges = Map()
                val outEdges = Map()
            }

            val members = agents.toList
        }

        val optBSP = BSPModel.Optimize.default(initPartition)

        benchmarkTool[Unit](
            Range(1, 5).foreach(_ => {
                optBSP.members.map(i => {
                    i.run(List())
                })
                // println(optBSP.members.map(m => m.state.asInstanceOf[(Array[BSP & ComputeMethod & Stage & DoubleBuffer], Option[PartitionMessage{type M = BSP; type Idx = BSPId}])]._1.map(j => j.state).mkString(", ")).mkString("\n"))
            })
        ) 
    }
}