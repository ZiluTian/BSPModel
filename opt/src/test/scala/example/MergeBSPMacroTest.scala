// package BSPModel
// package test

// import org.scalatest._
// import funsuite._

// import scala.reflect.runtime.universe._
// import java.lang.reflect.Field

// import scala.meta._

// import BSPModelMacros._

// // The equality of BSP objects is based on their runtime classes. 
// // Restrict users from passing type parameters when defining BSPs.
// class MergeBSPMacroTest extends AnyFunSuite {

//     trait CounterCompute extends ComputeMethod {
//         type State = Int
//         type Message = Int

//         def combineMessages(m1: List[Int]): Option[Int] = {
//             m1 match {
//                 case Nil => None
//                 case m :: Nil => Some(m)
//                 case m :: tail => Some(tail.fold(m)(_+_))
//             }
//         }

//         def updateState(s: Int, m: Option[Int]): Int = {
//             m match {
//                 case None => s
//                 case Some(x) =>                 
//                     s + x
//             }
//         }

//         def stateToMessage(s: Int): Int = {
//             s
//         }
//     }

//     class Cell(pos: BSPId, neighbors: Seq[BSPId]) extends BSP with CounterCompute {
//         var state: Int = 1
//         override val id = pos
//         val sendTo = FixedCommunication(neighbors) 
//     } 

//     val c1 = new Cell(1, List(2, 3, 4))
//     val c2 = new Cell(2, List(3, 4, 5))

//     // val bsps = List(c1, c2)
//     MergeBSP.combineStates(c1, c2)
// }