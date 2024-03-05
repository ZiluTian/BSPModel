package BSPModel

trait BSP {
    this: ComputeMethod =>
    // type S <: Scope
    // val scope: S

    val id: BSPId
    var state: State
    val sendTo: Iterable[BSPId]

    // Specialized kernel compiled from the AST
    def run(ms: List[Message]): Unit = {
        state = run(state, ms)
    }

    def message(): Message = {
        stateToMessage(state)
    }
}