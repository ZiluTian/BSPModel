package BSPModel

trait BSP {
    // self-type
    // https://docs.scala-lang.org/tour/self-types.html
    this: ComputeMethod =>

    val id: BSPId
    var state: State
    val receiveFrom: Iterable[BSPId]

    // Specialized kernel compiled from the AST
    def run(ms: Iterable[Message]): Unit = {
        state = run(state, ms)
    }

    def message(): Message = {
        stateToMessage(state)
    }
}