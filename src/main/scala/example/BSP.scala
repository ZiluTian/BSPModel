package BSPModel

import scala.collection.mutable.ArrayBuffer

// A BSP is a stateful object that couples ComputeMethod with a state
trait BSP {
    val compute: ComputeMethod

    type State = compute.State
    type Message = compute.Message

    var state: State
    var id: BSPId = BSP.getNextId()
    val outEdges: ArrayBuffer[BSPId] = new ArrayBuffer[BSPId]()

    // Specialized kernel compiled from the AST
    def run(ms: Seq[Message]): State = {
        compute.run(state, ms)
    }

    // The AST expression for updating the local state
    var ast: Expr[State] = Run[State](GetState[State](id), None)

    // For partial evaluation
    def curry(sid: BSPId): Unit = {
        ast = ast match {
            case Run(tr, None) => 
                Run[State](tr, Some(GetLocalValue(sid)))
            case Run(tr, Some(m: MessageExpr{type K=Message})) => 
                Run[State](tr, Some(CombineMessages[Message](GetLocalValue(sid), m)))
        }
    }
}

object BSP {
    private var lastId: BSPId = 0

    private def getNextId(): BSPId = this.synchronized {
        lastId = lastId + 1
        lastId
    }
}