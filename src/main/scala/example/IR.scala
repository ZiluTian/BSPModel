package BSPModel

import scala.collection.mutable.{Map => MutMap}

trait Expr[T]

trait MessageExpr {
    type K
}

case class GetState[S](x: BSPId) extends Expr[S]
case class GetMessage[M](x: BSPId) extends MessageExpr{type K = M}
case class GetLocalValue[M](x: BSPId) extends MessageExpr{type K = M}

case class CombineMessages[M](x: MessageExpr{type K=M}, y: MessageExpr{type K=M}) extends MessageExpr{type K=M}

case class Run[S](x: Expr[S], y: Option[MessageExpr]) extends Expr[S]