package out

import expr.Expr

trait ExpressionOutput[Effect[_] : Monad, Out] {
  def output(e: Expr): Effect[Out]
}
