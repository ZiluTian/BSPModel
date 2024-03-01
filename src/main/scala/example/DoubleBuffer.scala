package BSPModel

// DoubleBuffer is an optimization applied to a BSP
trait DoubleBuffer {
    this: BSP =>
    var publicState: Message
    
    override def run(ms: Seq[Message]): Unit = {
        state = compute.run(state, ms)
        publicState = compute.stateToMessage(state)
    }
}