package BSPModel

// User-defined Compute function
trait ComputeMethod {
    type State
    type Message

    // for expressing state update over aggregated message value
    // e.g. in gol, w.o combineMessages, State has to be defined as a pair
    def combineMessages(m1: Message, m2: Message): Message
    def updateState(s: State, m: Option[Message]): State
    def stateToMessage(s: State): Message

    def run(state: State, ms: Seq[Message]): State = {
        ms match {
            case Nil => updateState(state, None)
            case m :: Nil =>  updateState(state, Some(m))
            case m :: ms => updateState(state, Some(ms.foldLeft(m)((x, y) => combineMessages(x, y))))
        }
    }
}