package expr

import expr.Expr.*
import expr.Expr

object Derivative {
  def derivativeOf(expr: Expr, variable: Var): Expr = expr match {
    case `variable` => Const(1)
    // TODO
    case Expr.Var(name) => throw new RuntimeException("implement this when Derivative is added to Expr")
    case EffectivelyConstant(_) => Const(0)
    case Expr.Add(lhs, rhs) => Add(derivativeOf(lhs, variable), derivativeOf(rhs, variable))
    case Expr.Sub(lhs, rhs) => Sub(derivativeOf(lhs, variable), derivativeOf(rhs, variable))
    case m@Expr.Mul(EffectivelyConstant(_), rhs) => Mul(m.lhs, derivativeOf(rhs, variable))
    case m@Expr.Mul(lhs, EffectivelyConstant(_)) => Mul(m.rhs, derivativeOf(lhs, variable))
    case Expr.Mul(lhs, rhs) => Add(
      Mul(derivativeOf(lhs, variable), rhs),
      Mul(derivativeOf(rhs, variable), lhs)
    )
    // TODO, add support for x^e as well
    // represents x^n
    case Expr.Pow(expr, Const(n)) => Mul(Const(n), Expr.Pow(expr, Const(n - 1)))
    // represents e^(whatever else)
    case exp@Expr.Pow(Expr.e, exponent) => Mul(
      derivativeOf(exponent, variable),
      exp
    )
    // TODO
    case Expr.Pow(_, _) => throw new RuntimeException("Other forms of exponentiation have not been implemented yet")
    case Expr.Div(dividend, EffectivelyConstant(c)) => Div(
      derivativeOf(dividend, variable), c
    )
    // TODO
    case Expr.Div(EffectivelyConstant(_), divisor) => throw new RuntimeException("contant/exp has not been implemented yet")
    case Expr.Div(dividend, divisor) =>
      Div(
        Sub(
          Mul(
            derivativeOf(dividend, variable), divisor
          ),
          Mul(
            derivativeOf(divisor, variable), dividend
          )
        ),
        Pow(divisor, Const(2))
      )
    case Expr.Sin(arg) => Cos(arg)
    case Expr.Cos(arg) => throw new RuntimeException("implement this when Neg is there in Expr")
    case Expr.Tan(arg) => throw new RuntimeException("implement this when Sec and Csc are there in Expr")
    case Expr.Cot(arg) => throw new RuntimeException("implement this when Sec and Csc are there in Expr")
    case Expr.Func(name, arg) => throw new RuntimeException("Function support is not here yet")
    case c => throw new RuntimeException(s"Unrecognized case: $c")
  }
}
