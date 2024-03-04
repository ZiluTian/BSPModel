package BSPModel

// DoubleBuffer is an optimization applied to a BSP
trait DoubleBuffer {
    this: BSP with ComputeMethod =>
    var publicState: Message = stateToMessage(state)
    
    // runtime closures for staged expr
    var stagedExpr: Option[(Iterable[BSPId], (Iterable[BSPId]) => Option[Message])] = None

    override def run(ms: List[Message]): Unit = {
        state = run(state, ms)
        publicState = stateToMessage(state)
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