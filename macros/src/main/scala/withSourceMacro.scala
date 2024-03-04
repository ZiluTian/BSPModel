package BSPModelMacros

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

object FunctionWrapper {

  implicit def apply[P, R](fn: P => R): FunctionWrapper[P, R] = macro apply_impl[P, R]

  def apply_impl[P: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(fn: c.Expr[P => R]): 
                  c.Expr[FunctionWrapper[P, R]] = {
    import c.universe._
     c.Expr(q" new FunctionWrapper($fn, ${show(fn.tree)})")
  }
}

class FunctionWrapper[P, R](val fn: P => R, description: String) 
      extends Function1[P, R] {
  def apply(p: P) = fn(p)
  override def toString = description
}