package BSPModel

trait StagedExpr {
    type NodeId
    type Message

    val receiveFrom: List[NodeId]
    def compile(): Message
}
