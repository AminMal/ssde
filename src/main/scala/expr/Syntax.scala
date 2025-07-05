package expr

import scala.annotation.targetName

object Syntax {
  given numToConst: Conversion[Int, Expr] = Expr.Const(_)
  
  def variable(name: String): Expr.Var = Expr.Var(name)
  def sin(expr: Expr): Expr = Expr.Func("sin", expr)
  def cos(expr: Expr): Expr = Expr.Func("cos", expr)
  def tan(expr: Expr): Expr = Expr.Func("tan", expr)
  def cot(expr: Expr): Expr = Expr.Func("cot", expr)
  
  final val x: Expr.Var = variable("x")
  final val y: Expr.Var = variable("y")
  final val e: Expr = Expr.e
  
  extension (dis: Expr) {
    @targetName("sub")
    def -(that: Expr): Expr = Expr.Sub(dis, that)

    @targetName("add")
    def +(that: Expr): Expr = Expr.Add(dis, that)

    @targetName("mul")
    def *(that: Expr): Expr = Expr.Mul(dis, that)

    @targetName("div")
    def /(that: Expr): Expr = Expr.Div(dis, that)

    @targetName("pow")
    def ^^(that: Expr): Expr = Expr.Pow(dis, that)
    
    def simplified: Expr = Simplifier.simplify(dis)
    
    def derivativeOver(variable: Expr.Var): Expr =
      Derivative.derivativeOf(dis, variable)
      
    def neg: Expr = Expr.Neg(dis)
  }
}
