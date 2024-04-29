package BSPModel
package test

import scala.util.Random
import sourcecode._
import BSPModel.Util._

import org.scalatest._
import funsuite._

// A counter example that adds received messages to the current value
class ERMTest extends AnyFunSuite {

    val population: Int = 1000
    val connectivity: Double = 0.01
    val totalRounds: Int = 20

    val experimentName: String = "ERM"

    case class Person(val age: Int, 
                val symptomatic: Boolean,
                val health: Int,
                val vulnerability: Int, 
                val daysInfected: Int)

    trait ERMCompute extends ComputeMethod {
        type State = Person
        type Message = List[Double] // risk of being infected

        def partialCompute(m1: List[List[Double]]): Option[List[Double]] = {
            // println(f"Messages received are ${m1}")
            m1 match {
                case Nil => None
                case _ => Some(m1.flatten)
            }
        }

        def updateState(person: Person, m: Option[List[Double]]): Person = {
            if (person.health != SIRModel.Deceased) {
                var health: Int = person.health
                m match {
                    case None => 
                    case Some(risks) => 
                        risks.foreach(risk => {
                            var personalRisk = stateToMessage(person).head
                            if (person.age > 60) {
                                personalRisk = personalRisk * 2
                            }
                            if (personalRisk > 1) {
                                health = SIRModel.change(health, person.vulnerability)
                            }
                        })
                    }

                if ((health != SIRModel.Susceptible) && (health != SIRModel.Recover)) {
                    if (person.daysInfected >= SIRModel.stateDuration(health)) {                        
                        Person(person.age, person.symptomatic, SIRModel.change(health, person.vulnerability), person.vulnerability, 0)
                    } else {
                        Person(person.age, person.symptomatic, health, person.vulnerability, person.daysInfected + 1)
                    }
                } else {
                    person
                }
            } else {
                person
            }
        }

        def stateToMessage(s: Person): List[Double] = {
            if (s.health == SIRModel.Infectious) {
                List(SIRModel.infectiousness(s.health, s.symptomatic))
            } else {
                List(0)
            }
        }
    }

    class PersonAgent(pos: BSPId, neighbors: Seq[BSPId]) extends BSP with ERMCompute {
        val age: Int = Random.nextInt(90)+10
        var state: Person = Person(age, 
                Random.nextBoolean(), 
                if (Random.nextInt(100)==0) 0 else 2,
                vulnerability = if (age > 60) 1 else 0,
                0)
        override val id = pos
        val sendTo = FixedCommunication(neighbors) 
    } 

    test("Game of life example should change the state of population in every round") {
        val graph: Map[Long, Iterable[Long]] = (new ErdosRenyiGraph(population, connectivity)).g
        val agents = graph.map(i => new PersonAgent(i._1, i._2.toSeq))

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
            Optimizer.mergeBSP.transform(ans)
        }

        val ans = optimize(initPartition) 

        benchmarkTool[Unit](
            Range(1, totalRounds).foreach(_ => {
                ans.members.map(i => {
                    i.run(List())
                    // println(i.toString)
                })
            })
        ) 
    }
}