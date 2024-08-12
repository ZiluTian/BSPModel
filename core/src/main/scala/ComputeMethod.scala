package BSPModel

trait ComputeMethod {
    type State
    type Message

    // val opt: ComputeOptimization

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
    // change the value of initFoldValue inplace
    def statefulFold(ms: List[Message]): Unit

    override def partialCompute(ms: List[Message]): Option[Message] = {
        statefulFold(ms)
        None
    }

    override def run(state: State, ms: List[Message]): State = {
        partialCompute(ms)   
        updateState(state, None)
    }
}