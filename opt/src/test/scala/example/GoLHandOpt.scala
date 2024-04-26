package BSPModel
package test

import BSPModel.Util._

import org.scalatest._
import funsuite._
import scala.util.Random

// Hand-optimized counter example, to compare the performance of the result with
class handOptGoLTest extends AnyFunSuite {

    val width: Int = 100
    val height: Int = 100
    val totalRounds: Int = 5

    val experimentName: String = "game of life"

    test(f"The hand-optimized $experimentName example (shallow copy)"){

        var readOnly = Array.tabulate(width, height)((x, y) => Random.nextBoolean())
        var readWrite = readOnly.map(i => i.clone)

        benchmarkTool[Unit]{
            Range(1, totalRounds).foreach(_ => {
                Range(0, width).foreach(row => {
                    Range(0, height).foreach(col => {
                        var totalAlive: Int = 0

                        for {
                            i <- -1 to 1
                            j <- -1 to 1
                            if !(i == 0 && j == 0)
                                dx = (col + i + width) % width
                                dy = (row + j + height) % height
                        } {
                            if (readOnly(dx)(dy)) {
                                totalAlive += 1
                            }
                        }

                        if (totalAlive == 3) {
                            readWrite(row)(col) = true
                        } else if (totalAlive < 3 || totalAlive > 3) {
                            readWrite(row)(col) = false
                        } else {
                            readWrite(row)(col) = readOnly(row)(col)
                        }
                    })
                })
                // swap the pointers
                val tmp = readOnly
                readOnly = readWrite
                readWrite = tmp
            })
        }
    }
    
    test(f"The hand-optimized $experimentName example (deep copy)"){

        var readOnly = Array.tabulate(width, height)((x, y) => Random.nextBoolean())
        var readWrite = readOnly.map(i => i.clone)

        benchmarkTool[Unit]{
            Range(1, totalRounds).foreach(_ => {
                Range(0, width).foreach(row => {
                    Range(0, height).foreach(col => {
                        var totalAlive: Int = 0

                        for {
                            i <- -1 to 1
                            j <- -1 to 1
                            if !(i == 0 && j == 0)
                                dx = (col + i + width) % width
                                dy = (row + j + height) % height
                        } {
                            if (readOnly(dx)(dy)) {
                                totalAlive += 1
                            }
                        }

                        if (totalAlive == 3) {
                            readWrite(row)(col) = true
                        } else if (totalAlive < 3 || totalAlive > 3) {
                            readWrite(row)(col) = false
                        } else {
                            readWrite(row)(col) = readOnly(row)(col)
                        }
                    })
                })
                readOnly = readWrite.map(_.clone)
            })
        }
    }
}