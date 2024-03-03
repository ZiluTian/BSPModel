package BSPModel
package test

import scala.util.Random
import sourcecode._
import Util._

// A counter example that adds received messages to the current value
class CounterSpec extends munit.FunSuite {
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
        val width = 100
        val height = 100
        val g = new Torus2DGraph(width, height, 0)

        val agents = g.map(i => new Cell(i._1, i._2.toSeq))

        val initPartition = new Partition {
            val id = 1
            type NodeId = BSPId
            type Value = BSP
            val topo = Graph(agents.map(i => (i.id, i)).toMap, Map(), Map())
            
            def getMemberMessage[Int](k: BSPId): Int = {
                val bsp = topo.nodes(k)
                bsp.asInstanceOf[BSP with ComputeMethod].state.asInstanceOf[Int]
            }
        }

        // initPartition.topo.nodes.map()
        val ans = Optimizer.bspToDoubleBuffer.transform(initPartition)        
        // ans.topo.nodes.map(i => println(f"${i._2.state}, ${i._2.publicState}"))

        val ans2 = Optimizer.pushToPullAndUnbox.transform(ans)
        // ans2.topo.nodes.map(i => println(f"${i._2.sendTo}"))

        benchmarkTool[Unit](
            Range(1, 10).foreach(_ => {
                ans2.topo.nodes.map(i => {
                    i._2.run(List()) // 693382, 416672, 180646
                    // debug(i._2.state)
                })
            })
        )



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