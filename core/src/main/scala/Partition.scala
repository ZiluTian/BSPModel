package BSPModel

trait Partition extends Scope {
    val id: PartitionId
    // NodeId can be BSPId, but can also be vector id
    type NodeId
    type Member

    val topo: Graph[NodeId]
}