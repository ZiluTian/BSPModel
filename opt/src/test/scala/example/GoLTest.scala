// package BSPModel
// package test

// import scala.util.Random

// // Game of life example
// class GoLSpec extends munit.FunSuite {

//   class Cell(alive: Boolean, pos: BSPId, neighbors: Seq[BSPId]) extends BSP {
//     var state: Boolean = alive
//     override val id = pos
//     val sendTo = FixedCommunication(neighbors)

//     val compute = new ComputeMethod {
//       type State = Boolean
//       type Message = Int

//       def combineMessages(m1: Int, m2: Int): Int = {
//           m1 + m2
//       }

//       def updateState(s: Boolean, m: Option[Int]): Boolean = {
//           m match {
//               case None => s
//               case Some(x) =>                 
//                 if (s && (x > 3 || x < 2)) {
//                     false
//                 } else {
//                     true
//                 }
//           }
//       }

//       def stateToMessage(s: Boolean): Int = {
//           if (s) 1 else 0
//       }
//     }
//   }

//   test("say hello") {
//     val width = 10
//     val height = 10
//     val g = new Torus2DGraph(width, height, 0)

//     val initPartition = new BSPPartition {
//       val id = getNextId()

//       val members = 
//         g.map(i => {
//           val a = new Cell(Random.nextBoolean(), i._1, i._2.toSeq)
//           (i._1, BSP.toDoubleBuffer(a))
//         }).toList
      
//       val cuts = Map[PartitionId, List[BSPId]]()

//       val edges = members.map(idBSP => {
//             val extRefs = idBSP._2.sendTo.filter(x => !members.map(_._1).contains(x))
//             (idBSP._1, extRefs.toSeq)
//         }).groupBy(_._1).mapValues(_.flatMap(_._2).toList).toMap
    
//       println(edges)
//     }

//     val hbsp = new HierarchicalBSP {
//       val partitions = List(initPartition)
//       val optimizations = List(Localization)
//     }

//     // Localization.transform(initPartition)
//     val optHBSP = hbsp.optimize()
    
//     // initPartition.members.foreach(i => {
//     //   println(i._2.sendTo)
//     // })

//     // optHBSP.partitions.head.members.foreach(i => {
//     //   i.asInstanceOf[(Long, BSPPartition)]._2.members.foreach(i => println(i._2.sendTo))
//     // })
//   }
// }
