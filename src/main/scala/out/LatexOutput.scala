package out

import expr.Expr

object LatexOutput extends ExpressionOutput[Id, String] {
  private def toLatexString(e: Expr): String = e match {
    case Expr.Var(name) => name
    case Expr.Const(num) => num.toString
    case Expr.e => "e"
    case Expr.Add(lhs, rhs) => s"(${toLatexString(lhs)} + ${toLatexString(rhs)})"
    case Expr.Sub(lhs, rhs) => s"(${toLatexString(lhs)} - ${toLatexString(rhs)})"
    case Expr.Mul(lhs, rhs) => s"(${toLatexString(lhs)} * ${toLatexString(rhs)})"
    case Expr.Pow(base, exponent) => s"(${toLatexString(base)} ^ ${toLatexString(exponent)})"
    case Expr.Div(dividend, divisor) => s"\\frac{${toLatexString(dividend)}}{${toLatexString(divisor)}}"
    case Expr.Neg(e) => s"-${toLatexString(e)}"
    case Expr.Sin(arg) => s"sin(${toLatexString(arg)})"
    case Expr.Cos(arg) => s"cos(${toLatexString(arg)})"
    case Expr.Tan(arg) => s"tan(${toLatexString(arg)})"
    case Expr.Cot(arg) => s"cot(${toLatexString(arg)})"
    case Expr.Func(name, arg) => s"$name(${toLatexString(arg)})"
  }

  override def output(e: Expr): String = toLatexString(e)
}
