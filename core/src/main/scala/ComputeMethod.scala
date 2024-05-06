package BSPModel

// sealed trait ComputeOptimization
// case object StatelessAggregate extends ComputeOptimization
// case object InplacePartialAggregate extends ComputeOptimization

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

trait StatelessComputeMethod extends ComputeMethod {
    // inline val opt: ComputeOptimization = StatelessAggregate
}

// foldLeft allows for maintaining a state when aggregating messages, hence "stateful"
trait StatefulComputeMethod  extends ComputeMethod {
    // inline val opt: ComputeOptimization = InplacePartialAggregate

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