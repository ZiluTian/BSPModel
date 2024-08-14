package BSPModel

trait ComputeMethod {
    type State
    type Message

    def stateToMessage(s: State): Message
    def partialCompute(ms: Iterable[Message]): Option[Message]
    def updateState(s: State, m: Option[Message]): State
    
    def run(state: State, ms: Iterable[Message]): State = {
        updateState(state, partialCompute(ms))
    }
}

trait StatelessComputeMethod extends ComputeMethod 

// foldLeft allows for maintaining a state when aggregating messages, hence "stateful"
trait StatefulComputeMethod  extends ComputeMethod {
    // change the value of initFoldValue inplace
    def statefulFold(ms: Iterable[Message]): Unit

    override def partialCompute(ms: Iterable[Message]): Option[Message] = {
        statefulFold(ms)
        None
    }

    override def run(state: State, ms: Iterable[Message]): State = {
        partialCompute(ms)   
        updateState(state, None)
    }
}