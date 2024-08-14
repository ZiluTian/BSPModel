package BSPModel

// DoubleBuffer is a staging optimization applied to a BSP with ComputeMethod
// todo: restrict publicState to have read-only access to other BSPs but read-write only by the owner BSP
trait DoubleBuffer {
    this: BSP & ComputeMethod & Stage =>
    var publicState: Message
    
    def updatePublicState(): Unit = {
        publicState = stateToMessage(state)
    }
}