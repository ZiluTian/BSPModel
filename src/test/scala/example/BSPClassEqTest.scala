package BSPModel
package test

import org.scalatest._
import funsuite._
import scala.reflect.runtime.universe._

// The equality of BSP objects is based on their runtime classes. 
// Restrict users from passing type parameters when defining BSPs.
class BSPClassEqTest extends AnyFunSuite {
    trait CounterCompute extends ComputeMethod {
        type State = Int
        type Message = Int

        def combineMessages(m1: List[Int]): Option[Int] = ???
        def updateState(s: Int, m: Option[Int]): Int = ???
        def stateToMessage(s: Int): Int = ???
    }

    test("Instances of the same class definition are equal") {
        class Cell(pos: BSPId, neighbors: Seq[BSPId]) extends BSP with CounterCompute {
            var state: Int = 1
            override val id = pos
            val sendTo = FixedCommunication(neighbors) 
        }

        val c1 = new Cell(1, List(2, 3, 4))
        val c2 = new Cell(2, List(3, 4, 5))

        assert(c1.getClass() == c2.getClass())
        // no type parameter
        assert(c1.getClass.getTypeParameters().isEmpty)
    }

    test("Instances of the same class definition with different type members are equal") {
        class Cell[S, M](initState: S) extends ComputeMethod {
            type State = S
            type Message = M

            var state: S = initState
            val id = 10
            val sendTo = FixedCommunication(List(1, 2, 3))
            def combineMessages(m1: List[M]): Option[M] = ???
            def updateState(s: S, m: Option[M]): S = ???
            def stateToMessage(s: S): M = ???
        }

        val c1 = new Cell[Int, String](10)
        val c2 = new Cell[String, String]("hello")

        // User-facing agent definitions should not contain type parameters
        assert(c1.getClass() == c2.getClass())
        // Can distinguish by checking type parameters through reflecton
        assert(c1.getClass.getTypeParameters().nonEmpty)
    } 

    test("Instances of anonymous class definitions are not equal") {
        val c1 = new BSP with CounterCompute{
            var state: Int = 1
            override val id = 10
            val sendTo = FixedCommunication(List(1, 2, 3))
        }

        val c2 = new BSP with CounterCompute{
            var state: Int = 2
            override val id = 11
            val sendTo = FixedCommunication(List(1, 2, 3))
        }

        assert(c1.getClass() != c2.getClass())
    }   

    test("Instances of different class definitions are not equal") {
        class CellCounter(pos: BSPId, neighbors: Seq[BSPId]) extends BSP with CounterCompute {
            var state: Int = 1
            override val id = pos
            val sendTo = FixedCommunication(neighbors) 
        }

        class CellGoL(pos: BSPId, neighbors: Seq[BSPId]) extends BSP with CounterCompute {
            var state: Int = 1
            override val id = pos
            val sendTo = FixedCommunication(neighbors) 
        }

        val c1 = new CellCounter(1, List(2, 3, 4))
        val c2 = new CellGoL(2, List(3, 4, 5))

        assert(c1.getClass() != c2.getClass())
    }  
}