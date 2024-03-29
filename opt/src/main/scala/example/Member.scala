// package BSPModel

// import scala.reflect.runtime.universe._

// sealed trait Member

// trait BSP extends Member {
//     this: ComputeMethod =>
//     // type S <: Scope
//     // val scope: S

//     val id: BSPId
//     var state: State
//     val sendTo: Iterable[BSPId]

//     // Specialized kernel compiled from the AST
//     def run(ms: List[Message]): Unit = {
//         q"state = run(state, ms)"

//     }

//     def message(): Message = {
//         stateToMessage(state)
//     }
// }

// trait Partition extends Member{
//     val id: PartitionId
//     // NodeId can be BSPId, but can also be vector id
//     type NodeId
//     type Value

//     // Allow the graph structure to change dynamically
//     // for different instances of partitions
//     // class Graph(val nodes: Map[NodeId, Value],
//     //     val edges: Map[NodeId, List[NodeId]],
//     //     val cuts: Map[PartitionId, List[NodeId]])

//     val topo: Graph[NodeId, Value]
// }