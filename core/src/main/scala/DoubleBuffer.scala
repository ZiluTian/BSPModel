package BSPModel

// DoubleBuffer is an optimization applied to a BSP
// todo: restrict publicState to have read-only access to other BSPs but read-write only by the owner BSP
trait DoubleBuffer {
    this: BSP with ComputeMethod =>
    var publicState: Message = stateToMessage(state)
    
    // compile-time staging
    val stagedComputation: Option[StagedExpr]

    def updatePublicState(): Unit = {
        publicState = stateToMessage(state)
    }
    
    // todo: runtime staging to evaluate stagedComputation only once
    override def run(ms: List[Message]): Unit = {
        stagedComputation match {
            case None => state = run(state,  ms)
            case Some(x) => {
                val stagedRes = x.compile()
                println("staged expression evaluates to " + stagedRes)
                state = run(state, stagedRes.asInstanceOf[Message] :: ms)
            }
        }
    }
}

// todo, achieve so with reflection, to avoid creating a new object
object DoubleBuffer {
    def fromBSP(b: BSP with ComputeMethod): BSP with ComputeMethod with DoubleBuffer = {
        new BSP with ComputeMethod with DoubleBuffer {

            type State = b.State
            type Message = b.Message

            val id = b.id
            var state = b.state
            val sendTo = b.sendTo

            val stagedComputation = None
            
            def combineMessages(ms: List[Message]): Option[Message] = b.combineMessages(ms)
            
            def stateToMessage(s: State): Message = {
                b.stateToMessage(s)
            }

            def updateState(s: State,m: Option[Message]): State = {
                b.updateState(s, m)
            }
        }
    }
}