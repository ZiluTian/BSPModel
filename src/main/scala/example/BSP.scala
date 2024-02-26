package BSPModel

import scala.collection.mutable.ArrayBuffer

// A BSP is a stateful object that couples ComputeMethod with a state
trait BSP {
    val compute: ComputeMethod

    type State = compute.State
    type Message = compute.Message

    // initial state
    val state: State
     
    var id: BSPId = getNextId()
    // mutable, can be changed dynamically via rewriting
    val outEdges: ArrayBuffer[BSPId] = new ArrayBuffer[BSPId]()

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
}