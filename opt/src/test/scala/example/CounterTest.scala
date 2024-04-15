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

        def combineMessages(m1: List[Int]): Option[Int] = {
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
        val width = 10
        val height = 10
        val g = new Torus2DGraph(width, height, 0)

        val agents = g.map(i => new Cell(i._1, i._2.toSeq))

        val initPartition = new Partition {
            type Member = BSP & ComputeMethod
            type NodeId = BSPId
            type Value = BSP
            val id = 1

            val topo = Graph(Map(), Map())
            val members = agents.toList
        }

        // initPartition.topo.nodes.map()
        val ans = Optimizer.bspToDoubleBuffer.transform(initPartition)        
        // ans.topo.nodes.map(i => println(f"${i._2.state}, ${i._2.publicState}"))

        val ans2 = Optimizer.pushToPullAndUnbox.transform(ans)
        // ans2.topo.nodes.map(i => println(f"${i._2.sendTo}"))

        // Range(1, 5).foreach(_ => {
        //   ans2.topo.nodes.foreach(i => {
        //     i._2.run(List())
        //   })  
          
        //   ans2.topo.nodes.foreach(i => {
        //     i._2.updatePublicState() 
        //   })

        //   println("State is " + ans2.topo.nodes.map(_._2.state).mkString(", "))
        //   println("Public state is " + ans2.topo.nodes.map(_._2.publicState).mkString(", "))
        // })

        // ans2.topo.nodes.map(i => println(i._2.state))
        // ans2.topo.nodes.map(i => println(i._2.publicState))

        val ans3 = Optimizer.mergeBSP.transform(ans2)

        // println(ans2.topo.nodes.head._2)
        // benchmarkTool[Unit](
            Range(1, 5).foreach(_ => {
                ans3.members.map(i => {
                    i.run(List()) // 693382, 416672, 180646
                    // println(i._2.state)
                    // println(i._2.publicState)
                    println(i.toString)
                })
            })
        // )



        // initPartition.topo.nodes.map(i => println(f"${i._2.state}, ${i._2.publicState}"))

        // val initPartition = new BSPPartition {
        //     val id = getNextId()

        //     val members = 
        //         g.map(i => {
        //         val a = new Cell(i._1, i._2.toSeq)
        //         (i._1, BSP.toDoubleBuffer(a))
        //         }).toList
            
        //     val cuts = Map[PartitionId, List[BSPId]]()

        //     val edges = members.map(idBSP => {
        //         val extRefs = idBSP._2.sendTo.filter(x => !members.map(_._1).contains(x))
        //         (idBSP._1, extRefs.toSeq)
        //     }).groupBy(_._1).mapValues(_.flatMap(_._2).toList).toMap
        // }


        // val hbsp = new HierarchicalBSP {
        //     val partitions = List(initPartition)
        //     val optimizations = List(Localization)
        // }

        // // Localization.transform(initPartition)
        // val optHBSP: HierarchicalBSP = hbsp.optimize()
        // optHBSP.partitions.foreach(p => {
        //     p.members.foreach(i => {
        //         i._2.asInstanceOf[BSP].run(List())
        //     })
        // })
    }
}