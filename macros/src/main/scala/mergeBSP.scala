package BSPModelMacros

import BSPModel._

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

object MergeBSP {
  def combineStates(bsps: BSP with ComputeMethod*): BSP = macro combineStatesImpl

  def combineStatesImpl(c: Context)(bsps: c.Expr[BSP with ComputeMethod]*): c.Expr[BSP with ComputeMethod] = {
    import c.universe._

    val states = bsps.map(b => q"${b}.state")
    
    val combinedState = q"Array(..$states)"
    
    // extract the type of state from the first BSP object
    val stateType = bsps.head.actualType.decl(TermName("state")).typeSignature match {
      case NullaryMethodType(resultType) => resultType
      case other => c.abort(c.enclosingPosition, s"Unexpected type signature for state: $other")
    }

    val computeParams = List(q"s: Array[$stateType]", q"m: List[Message]")
    val computeBody = q"s.foreach(x => {x = ${bsps.head}.compute(x, m)})"

    // Find the CounterCompute trait in the BSP object expression's type hierarchy
    val counterComputeTrait = bsps.head.actualType.baseClasses
      .map(_.asClass)
      .find(_.fullName == typeOf[ComputeMethod].typeSymbol.fullName)
      .getOrElse(c.abort(c.enclosingPosition, "CounterCompute trait not found in BSP object expression"))

    // Extract the expressions for updateState and combineMessages methods from the CounterCompute trait
    val updateStateExpr = counterComputeTrait.typeSignature.decls.collectFirst {
      case method if method.name.toString == "updateState" => method.asMethod
    }
    // .map { method =>
    //     println(method)
    //   q"override def updateState(s: ${method.paramLists.head.head.typeSignature}, m: Option[${method.paramLists.head.last.typeSignature}]): ${method.returnType} = ???" // You can replace "???" with the actual body expression
    // }.getOrElse(c.abort(c.enclosingPosition, "updateState method not found"))

    println("Update state expression: " + updateStateExpr)

    val combineMessagesExpr = counterComputeTrait.typeSignature.decls.collectFirst {
      case method if method.name.toString == "combineMessages" => method.asMethod
    }.map { method =>
      q"override def combineMessages(m1: ${method.paramLists.head.head.typeSignature}): ${method.returnType} = ???" // You can replace "???" with the actual body expression
    }.getOrElse(c.abort(c.enclosingPosition, "combineMessages method not found"))


    // val updateStateExpr = bsps.head.tree match {
    //   case q"new $_ { ..$body }" =>
    //     body.collectFirst {
    //       case DefDef(_, TermName("updateState"), _, _, _, body) => body
    //     }.getOrElse(c.abort(c.enclosingPosition, "updateState method not found"))
    //   case _ => c.abort(c.enclosingPosition, "Invalid BSP object expression")
    // }

    // val combineMsgsExpr = bsps.head.tree.collect {
    //   case DefDef(_, TermName("combineMessages"), _, _, _, body) => body
    // }.headOption.getOrElse(c.abort(c.enclosingPosition, "compute method not found"))

    // println(combineMsgsExpr)

    val result = q"""trait VectCompute extends BSP with ComputeMethod {
      type State = Array[$stateType]
      type Message = PartitionMessage{type M = $stateType; type Idx = PartitionId}

      var state: State = $combinedState
      val id = newId()
      def compute(..$computeParams): Array[$stateType] = $computeBody
    };
    """

    // println(result)
    c.Expr[BSP with ComputeMethod](result)
  }
}