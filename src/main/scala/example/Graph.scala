package BSPModel

case class Graph[NodeId, Value](
    val nodes: Map[NodeId, Value],
    val edges: Map[NodeId, List[NodeId]],
    val cuts: Map[PartitionId, List[NodeId]]
)