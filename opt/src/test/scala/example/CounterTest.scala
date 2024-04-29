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

        def partialCompute(m1: List[Int]): Option[Int] = {
            m1 match {
                case Nil => None
                case m :: Nil => Some(m)
                case m :: tail => Some(tail.fold(m)(_+_))
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
        val sendTo = FixedCommunication(neighbors) 
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

            val topo = Graph(Map(), Map())
            val members = agents.toList
        }

        def optimize(part: Partition{type Member = BSP & ComputeMethod; type NodeId = BSPId}): Partition{type Member = BSP & ComputeMethod; type NodeId = BSPId} = {
            // val ans = Optimizer.bspToDoubleBuffer.transform(part) 
            val ans2 = Optimizer.bspToDoubleBuffer.transform(part)
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