package out

import expr.Expr

object LatexOutput extends ExpressionOutput[Id, String] {

  private object NotAddOrSub {
    def unapply(e: Expr): Option[Expr] = e match {
      case Expr.Add(_, _) | Expr.Sub(_, _) => None
      case _                               => Some(e)
    }
  }

  private object VarComesNext {
    def unapply(e: Expr): Option[Expr] = e match {
      case Expr.Var(_) | Expr.Pow(Expr.Var(_), _) | Expr.e => Some(e)
      case _                                                                 => None
    }
  }

  private def toLatexString(e: Expr): String = e match {
    case Expr.Var(name) => name.toString
    case Expr.Const(num) => s"{$num}"
    case Expr.e => "e"
    case Expr.Add(lhs, rhs) => s"(${toLatexString(lhs)} + ${toLatexString(rhs)})"
    case Expr.Sub(lhs, rhs) => s"(${toLatexString(lhs)} - ${toLatexString(rhs)})"
    case Expr.Mul(lhs, rhs@Expr.Func(_, _)) => s"${toLatexString(lhs)}${toLatexString(rhs)}"
    case Expr.Mul(lhs@Expr.Func(_, _), rhs) => s"${toLatexString(rhs)}${toLatexString(lhs)}"
    case Expr.Mul(lhs, VarComesNext(rhs)) => s"${toLatexString(lhs)}${toLatexString(rhs)}"
    case Expr.Mul(VarComesNext(lhs), rhs) => s"${toLatexString(rhs)}${toLatexString(lhs)}"
    case Expr.Mul(NotAddOrSub(lhs), NotAddOrSub(rhs)) => s"${toLatexString(lhs)} * ${toLatexString(rhs)}"
    case Expr.Mul(lhs, rhs) => s"(${toLatexString(lhs)} * ${toLatexString(rhs)})"
    case Expr.Pow(base, exponent) => s"${toLatexString(base)} ^ ${toLatexString(exponent)}"
    case Expr.Div(dividend, divisor) => s"\\frac{${toLatexString(dividend)}}{${toLatexString(divisor)}}"
    case Expr.Neg(e) => s"-${toLatexString(e)}"
    case Expr.Func(name, arg) => s"$name(${toLatexString(arg)})"
  }

  override def output(e: Expr): String = toLatexString(e)
}
