package BSPModel
package test

import scala.util.Random
import sourcecode._
import BSPModel.Util._

import org.scalatest._
import funsuite._

// A counter example that adds received messages to the current value
class GoLTest extends AnyFunSuite {
    trait GoLCompute extends ComputeMethod {
        type State = Boolean
        type Message = Int

        def combineMessages(m1: List[Int]): Option[Int] = {
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
        val sendTo = FixedCommunication(neighbors) 
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

            val topo = Graph(Map(), Map())
            val members = agents.toList
        }

        def optimize(part: Partition{type Member = BSP & ComputeMethod; type NodeId = BSPId}): Partition{type Member = BSP & ComputeMethod; type NodeId = BSPId} = {
            val ans = Optimizer.bspToDoubleBuffer.transform(part) 
            val ans2 = Optimizer.pushToPullAndUnbox.transform(ans)
            Optimizer.mergeBSP.transform(ans2)
        }

        val ans = optimize(initPartition) 

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