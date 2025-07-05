package expr

import expr.Expr.*

object Simplifier {
  def simplify(ex: Expr): Expr = ex match {
    case Const(_)                                                           => ex
    case Var(_)                                                             => ex
    case `e`                                                                => ex
    case Neg(Const(0))                                                      => Const(0)
    case Neg(Neg(inner))                                                    => inner
    case Add(lhs, Neg(rhs))                                                 => Sub(lhs, rhs)
    case Add(Neg(lhs), rhs)                                                 => Sub(rhs, lhs)
    case Add(lhs, rhs)                                                      =>
      val simplifiedLhs = simplify(lhs)
      val simplifiedRhs = simplify(rhs)
      if simplifiedLhs == Const(0) then simplifiedRhs
      else if simplifiedRhs == Const(0) then simplifiedLhs
      else Add(simplifiedLhs, simplifiedRhs)
    case Mul(lhs, rhs)                                                      =>
      val simplifiedLhs = simplify(lhs)
      val simplifiedRhs = simplify(rhs)
      if simplifiedLhs == Const(1) then simplifiedRhs
      else if simplifiedLhs == Const(0) || simplifiedRhs == Const(0) then Const(0)
      else if simplifiedRhs == Const(1) then simplifiedLhs
      else Mul(simplifiedLhs, simplifiedRhs)
    case Pow(base, exponent)                                                =>
      val simplifiedBase = simplify(base)
      val simplifiedExponent = simplify(exponent)
      if simplifiedExponent == Const(0) then Const(1)
      else if simplifiedExponent == Const(1) then simplifiedBase
      else if simplifiedBase == Const(0) then Const(0)
      else if simplifiedBase == Const(1) then Const(1)
      else Pow(simplifiedBase, simplifiedExponent)
    case Div(dividend, divisor)                                             =>
      val simplifiedDividend = simplify(dividend)
      val simplifiedDivisor  = simplify(divisor)
      if simplifiedDivisor == Const(1) then simplifiedDividend
      else if simplifiedDividend == simplifiedDivisor then Const(1)
      else Div(simplifiedDividend, simplifiedDivisor)
    case Func(name, arg)                                                    => Func(name, simplify(arg))
    case other                                                              => other
  }
}
