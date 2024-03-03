package BSPModel

import scala.collection.mutable.ArrayBuffer

// Box for rewriting. Unbox during optimization
trait OutNeighbors extends Iterable[BSPId]

case class FixedCommunication(xs: Seq[BSPId]) extends OutNeighbors {
    def iterator = xs.iterator
}

case class DynamicCommunication(xs: ArrayBuffer[BSPId]) extends OutNeighbors {
    def iterator = xs.iterator
}

case class HybridCommunication(xs: FixedCommunication, ys: DynamicCommunication) extends OutNeighbors {
    def iterator = xs.iterator ++ ys.iterator
}

object OutNeighbors {
    def removeStatic(b: BSP with ComputeMethod with DoubleBuffer): BSP with ComputeMethod with DoubleBuffer = {
        new BSP with ComputeMethod with DoubleBuffer{

            type State = b.State
            type Message = b.Message

            val id = b.id
            var state = b.state
            val sendTo = b.sendTo match {
                case FixedCommunication(_) => List()
                case DynamicCommunication(xs) => xs
                case HybridCommunication(xs, ys) => ys.xs
                case _ => {
                    b.sendTo                    
                } 
            }

            def combineMessages(ms: List[Message]): Option[Message] = {
                b.combineMessages(ms)
            }

            def stateToMessage(s: State): Message = {
                b.stateToMessage(s)
            }

            def updateState(s: State,m: Option[Message]): State = {
                b.updateState(s, m)
            }
        }
    }
}