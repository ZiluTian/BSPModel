package BSPModel

import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.{universe => ru}

// A BSP is a stateful object that couples ComputeMethod with a state
trait BSP {
    import BSP._

    val compute: ComputeMethod

    type State = compute.State
    type Message = compute.Message

    // Communicate with same neighbors per superstep
    // Can compile away (replace with empty neighbors) after generating specialized BSP
    type FixedCommunication = Seq[BSPId]
    // Communicate with different neighbors per superstep
    type DynamicCommunication = ArrayBuffer[BSPId]

    // initial state
    val state: State
    val id: BSPId = getNextId()
    val sendTo: OutNeighbors

    // Specialized kernel compiled from the AST
    def run(ms: Seq[Message]): State = {
        compute.run(state, ms)
    }

    // The AST expression for ms, combined with compute.combineMessages
    var ast: Option[MessageExpr] = None

    // Expr[State] => State => in-place update
    def exec(): BSP = ??? 

    // Stage read operation to obtain the value from sid
    def stageRead(sid: BSPId): Unit = {
        ast = ast match {
            case None => Some(GetLocalValue[Message](sid))
            case Some(a: MessageExpr{type K=Message}) => Some(CombineMessages[Message](GetLocalValue(sid), a))
        }
    }

    implicit def liftSendTo(x: FixedCommunication): Either[FixedCommunication, Nothing] = Left(x)
    implicit def liftSendTo(x: DynamicCommunication): Either[Nothing, DynamicCommunication] = Right(x)
}

object BSP {
    def fixedCommunication(bsp: BSP): Boolean = {
        bsp.sendTo match {
            case FixedCommunication(x) => true
            case _ => false
        }
    }
}