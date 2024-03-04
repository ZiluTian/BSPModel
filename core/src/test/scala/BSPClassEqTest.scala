package BSPModel
package test

import org.scalatest._
import funsuite._

import scala.reflect.runtime.universe._
import java.lang.reflect.Field

import scala.meta._

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

    test("Instances of the same class definition are equal, even with different run methods") {
        class Cell(pos: BSPId, neighbors: Seq[BSPId], custRun: (Int, Option[Int])=>Int) extends BSP with CounterCompute {
            var state: Int = 1
            override val id = pos
            val sendTo = FixedCommunication(neighbors) 

            override def updateState(s: Int, m: Option[Int]): Int = custRun(s, m)
        }

        val c1 = new Cell(1, List(2, 3, 4), (x: Int, y: Option[Int]) => x + 1)
        val c2 = new Cell(2, List(3, 4, 5), (x: Int, y: Option[Int]) => x + 2)
        val c3 = new Cell(2, List(3, 4, 5), (x: Int, y: Option[Int]) => x + 2)

        assert(c1.getClass() == c2.getClass())
        // no type parameter
        assert(c1.getClass.getTypeParameters().isEmpty)
        // cannot distinguish c1 and c2 through runtime reflection
        val mirror = runtimeMirror(getClass.getClassLoader())
        val runMethod1 = mirror.classSymbol(c1.getClass).toType.decl(TermName("updateState")).alternatives.map(i => i.asMethod)
        val runMethod2 = mirror.classSymbol(c2.getClass).toType.decl(TermName("updateState")).alternatives.map(i => i.asMethod)
        assert(runMethod1 == runMethod2)
        // check the memory address of the updateState methods instead
        // assert(System.identityHashCode(c1.updateState) != System.identityHashCode(c2.updateState))
        val unsafe: sun.misc.Unsafe = {
            val field: java.lang.reflect.Field = classOf[sun.misc.Unsafe].getDeclaredField("theUnsafe")
            field.setAccessible(true)
            field.get(null).asInstanceOf[sun.misc.Unsafe]
        }

        def getMemAddress(obj: Any): Long = unsafe.objectFieldOffset(obj.getClass.getDeclaredField("custRun"))
        // The memory object is actually the same!
        // assert(getMemAddress(c1) != getMemAddress(c2))
        
        // def lambdaToString(lambda: AnyRef): String = {
        //     val mirror = runtimeMirror(lambda.getClass.getClassLoader)
        //     val methodSymbol = mirror.classSymbol(lambda.getClass).toType.members.find(_.isMethod).get.asMethod

        //     // val methodMirror = mirror.reflect(lambda).reflectMethod(methodSymbol)
        //     // val methodSource = methodMirror.symbol.owner.asClass.sourceCode

        //     val methodTree = q"${methodSymbol.name.toTermName}"

        //     showCode(methodTree)
        // }

        // toString only returns a representation of the lambda function, the identifier for the lambda function
        // println(c1.updateState _ toString())
        // println(c2.updateState _ toString())
        // println(c3.updateState _ toString())
    
        // val lambda = FunctionWrapper((x: Int) => x + 3)
        // println(s"lambda function is $lambda, eveluated at 3 is ${lambda(3, 4)}")
        // val lambdaString = lambdaToString(lambda)
        // println(lambdaString)
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