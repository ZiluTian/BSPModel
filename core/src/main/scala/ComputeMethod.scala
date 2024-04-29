package BSPModel

trait ComputeMethod {
    type State
    type Message

    def stateToMessage(s: State): Message
    def partialCompute(ms: List[Message]): Option[Message]
    def updateState(s: State, m: Option[Message]): State
    
    def run(state: State, ms: List[Message]): State = {
        updateState(state, partialCompute(ms))
    }
}

trait StatelessComputeMethod extends ComputeMethod 

// foldLeft allows for maintaining a state when aggregating messages, hence "stateful"
trait StatefulComputeMethod  extends ComputeMethod {
    // foldLeft allows for incrementally adjusting the internal state
    var initFoldValue: Message
    def statefulFold(init: Message)(ms: List[Message]): Message

    override def partialCompute(ms: List[Message]): Option[Message] = {
        val x = statefulFold(initFoldValue)(ms)
        initFoldValue = x
        Some(x)
    }
}