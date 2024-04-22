package BSPModel
package test

import BSPModel.Util._

import org.scalatest._
import funsuite._

// Hand-optimized counter example, to compare the performance of the result with
class handOptCounterTest extends AnyFunSuite {

    val width: Int = 100
    val height: Int = 100
    val totalRounds: Int = 5

    test("The hand-optimized counter example"){

        var readOnly = Array.tabulate(width, height)((x, y) => 1)
        var readWrite = Array.tabulate(width, height)((x, y) => 1)

        benchmarkTool[Unit]{
            Range(1, totalRounds).foreach(_ => {
                Range(0, width).foreach(row => {
                    Range(0, height).foreach(col => {
                        for {
                            i <- -1 to 1
                            j <- -1 to 1
                            if !(i == 0 && j == 0)
                                dx = (col + i + width) % width
                                dy = (row + j + height) % height
                        } {
                            readWrite(row)(col) += readOnly(dx)(dy)
                        }
                    })
                })
                // readOnly.foreach(l => println(l.toList))
                readOnly = readWrite.map(_.clone)
            })
        }
    }
}