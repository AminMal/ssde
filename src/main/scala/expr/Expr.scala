package expr

import errors.EvaluationError

enum Expr {
  case Var(name: String)
  case Const(num: Int)
  case e
  case Add(lhs: Expr, rhs: Expr)
  case Sub(lhs: Expr, rhs: Expr)
  case Mul(lhs: Expr, rhs: Expr)
  case Pow(base: Expr, exponent: Expr)
  case Div(dividend: Expr, divisor: Expr)
  case Neg(e: Expr)
  case Sin(arg: Expr)
  case Cos(arg: Expr)
  case Tan(arg: Expr)
  case Cot(arg: Expr)
  case Func(name: String, arg: Expr)

  def show: String = this match {
    case Expr.Var(name) => name
    case Expr.Const(num) => num.toString
    case Expr.e => "e"
    case Expr.Add(lhs, rhs) => s"(${lhs.show} + ${rhs.show})"
    case Expr.Sub(lhs, rhs) => s"(${lhs.show} - ${rhs.show})"
    case Expr.Mul(lhs, Var(rhs)) => s"${lhs.show}$rhs"
    case Expr.Mul(Var(lhs), rhs) => s"${rhs.show}$lhs"
    case Expr.Mul(lhs, rhs) => s"(${lhs.show} * ${rhs.show})"
    case Expr.Pow(base, exponent) => s"${base.show}^${exponent.show}"
    case Expr.Div(dividend, divisor) => s"${dividend.show}/${divisor.show}"
    case Expr.Neg(x) => s"-(${x.show})"
    case Expr.Sin(arg) => s"sin(${arg.show})"
    case Expr.Cos(arg) => s"cos(${arg.show})"
    case Expr.Tan(arg) => s"tan(${arg.show})"
    case Expr.Cot(arg) => s"cot(${arg.show})"
    case Expr.Func(name, arg) => s"$name(${arg.show})"
  }
  
  def isEffectivelyConstant: Boolean = this match {
    case Expr.Const(_) | `e` => true
    case Expr.Neg(x) => x.isEffectivelyConstant
    case Expr.Sin(x) => x.isEffectivelyConstant
    case Expr.Cos(x) => x.isEffectivelyConstant
    case Expr.Tan(x) => x.isEffectivelyConstant
    case Expr.Cot(x) => x.isEffectivelyConstant
    case Expr.Add(x, y) => x.isEffectivelyConstant && y.isEffectivelyConstant
    case Expr.Sub(x, y) => x.isEffectivelyConstant && y.isEffectivelyConstant
    case Expr.Mul(x, y) => x.isEffectivelyConstant && y.isEffectivelyConstant
    case Expr.Div(x, y) => x.isEffectivelyConstant && y.isEffectivelyConstant
    case Expr.Pow(x, y) => x.isEffectivelyConstant && y.isEffectivelyConstant
    case _ => false
  }

  def solveFor(args: (String, Double)*): Double = {
    val argsRegistry = args.toMap
    this match {
      case Expr.Var(name) => argsRegistry.getOrElse(name, throw EvaluationError.VariableNotFound(name))
      case Expr.Const(num) => num.toDouble
      case Expr.e => 2.71828
      case Expr.Add(lhs, rhs) => lhs.solveFor(args*) + rhs.solveFor(args*)
      case Expr.Sub(lhs, rhs) => lhs.solveFor(args*) - rhs.solveFor(args*)
      case Expr.Mul(lhs, rhs) => lhs.solveFor(args*) * rhs.solveFor(args*)
      case Expr.Pow(base, exponent) => Math.pow(base.solveFor(args*), exponent.solveFor(args*))
      case Expr.Div(dividend, divisor) => dividend.solveFor(args*) / divisor.solveFor(args*)
      case Expr.Neg(e) => - e.solveFor(args*)
      case Expr.Sin(arg) => Math.sin(arg.solveFor(args*))
      case Expr.Cos(arg) => Math.cos(arg.solveFor(args*))
      case Expr.Tan(arg) => Math.tan(arg.solveFor(args*))
      case Expr.Cot(arg) => 1d / Math.tan(arg.solveFor(args*))
      case Expr.Func(name, arg) => ???
    }
  }
}

object Expr {
  object EffectivelyConstant {
    def unapply(e: Expr): Option[Expr] = if e.isEffectivelyConstant then Some(e) else None
  }
  
  def func(name: String, arg: Expr): Expr = name.toLowerCase match {
    case "sin" => Expr.Sin(arg)
    case "cos" => Expr.Cos(arg)
    case "tan" => Expr.Tan(arg)
    case "cot" => Expr.Cot(arg)
    case _     => Expr.Func(name, arg)
  }
}