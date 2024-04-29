package BSPModel

// DoubleBuffer is an optimization applied to a BSP
// todo: restrict publicState to have read-only access to other BSPs but read-write only by the owner BSP
trait DoubleBuffer {
    this: BSP & ComputeMethod =>
    var publicState: Message
    
    // runtime staging (JIT compiled per run)
    val stagedComputation: Option[StagedExpr]

    def updatePublicState(): Unit = {
        publicState = stateToMessage(state)
    }

    override def run(ms: List[Message]): Unit = {
        stagedComputation match {
            case None => state = run(state,  ms)
            case Some(x) => {
                val stagedRes = x.compile()
                // println("staged expression evaluates to " + stagedRes)
                state = run(state, stagedRes.asInstanceOf[Message] :: ms)
            }
        }
    }
}