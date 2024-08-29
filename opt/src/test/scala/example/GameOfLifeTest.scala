package BSPModel
package test

import scala.util.Random
import sourcecode._
import BSPModel.Util._

import org.scalatest._
import funsuite._

// A counter example that adds received messages to the current value
class GoLTest extends AnyFunSuite {
    trait GoLCompute extends StatelessComputeMethod {
        type State = Boolean
        type Message = Int

        def partialCompute(m1: Iterable[Int]): Option[Int] = {
            // println(f"Messages received are ${m1}")
            m1 match {
                case Nil => None
                case _ => Some(m1.fold(0)(_+_))
            }
        }

        def updateState(s: Boolean, m: Option[Int]): Boolean = {
            m match {
                case None => {
                    s
                }
                case Some(totalAlive) =>     
                    if (totalAlive == 3) {
                        true
                    } else if (totalAlive < 3 || totalAlive > 3) {
                        false
                    } else {
                        s
                    }
            }
        }

        def stateToMessage(s: Boolean): Int = {
            if (s) 1 else 0
        }
    }

    class Cell(pos: BSPId, neighbors: Seq[BSPId]) extends BSP with GoLCompute {
        var state: Boolean = Random.nextBoolean()
        override val id = pos
        val receiveFrom = FixedCommunication(neighbors) 
    } 

    test("Game of life example should change the state of population in every round") {
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
                val edges = g.map(i => (i._1, i._2.toList))
                val inEdges = Map()
                val outEdges = Map()
            }

            val members = agents.toList
        }

        val ans = BSPModel.Optimize.default(initPartition)

        benchmarkTool[Unit](
            Range(1, 5).foreach(_ => {
                ans.members.map(i => {
                    i.run(List())
                    // println(i.toString)
                })
            })
        ) 
    }
}