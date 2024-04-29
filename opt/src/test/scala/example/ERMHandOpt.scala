package BSPModel
package test

import BSPModel.Util._

import org.scalatest._
import funsuite._
import scala.util.Random

// Hand-optimized counter example, to compare the performance of the result with
class handOptERMTest extends AnyFunSuite {

    val population: Int = 1000
    val connectivity: Double = 0.01
    val totalRounds: Int = 20

    val experimentName: String = "ERM"
    
    case class Person(val age: Int, 
                    val neighbors: Iterable[Int], 
                    val symptomatic: Boolean,
                    var health: Int,
                    var vulnerability: Int, 
                    var daysInfected: Int, 
                    var risk: Double) 

    test(f"The hand-optimized $experimentName example (deep copy)"){

        val graph: Map[Int, Iterable[Int]] = (new ErdosRenyiGraph(population, connectivity)).g.map(i => (i._1.toInt, i._2.map(j => j.toInt)))

        var readOnly: Array[Person] = (0 to population-1).map(i => {
            val age: Int = Random.nextInt(90)+10
            Person(age, 
                graph.getOrElse(i, throw new Exception(f"Error! ${i} not found")), 
                Random.nextBoolean(), 
                if (Random.nextInt(100)==0) 0 else 2,
                vulnerability = if (age > 60) 1 else 0,
                0,
                0)
        }).toArray

        var readWrite = readOnly.clone

        benchmarkTool[Unit]{
            Range(1, totalRounds).foreach(_ => {
                readOnly.zipWithIndex.foreach(pair => {
                    val person = pair._1
                    var health = person.health 
                    if (health != SIRModel.Deceased) {
                        // use reduce instead of combine, to allow for stateful updates, where the 
                        // initial value passed to reduce is stateful
                         
                        health = person.neighbors.map(i => readOnly(i)).foldLeft(health)((x, y) => {
                            var personalRisk = y.risk
                            if (person.age > 60) {
                                personalRisk = 2 * personalRisk
                            }
                            if (personalRisk > 1) {
                                SIRModel.change(health, person.vulnerability)
                            } else {
                                health
                            }
                        })

                        // person.neighbors.foreach(n => {
                        //     var personalRisk = readOnly(n).risk
                        //     if (person.age > 60) {
                        //         personalRisk = personalRisk * 2
                        //     }
                        //     if (personalRisk > 1) {
                        //         health = SIRModel.change(health, person.vulnerability)
                        //     }
                        // })

                        if (health == SIRModel.Infectious) {
                            readWrite(pair._2).risk = SIRModel.infectiousness(health, person.symptomatic)
                        }

                        if ((health != SIRModel.Susceptible) && (health != SIRModel.Recover)) {
                            if (person.daysInfected >= SIRModel.stateDuration(health)) {
                                health = SIRModel.change(health, person.vulnerability)
                                readWrite(pair._2).daysInfected = 0
                            } else {
                                readWrite(pair._2).daysInfected += 1
                            }
                        }
                        readWrite(pair._2).health = health
                    } 
                })
                // println(f"Summary: ${readOnly.groupBy(i => i.health).map(i => (i._1, i._2.size))}")
                readOnly = readWrite.clone
            })
        }
    }
}