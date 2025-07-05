package parser

import errors.ParseError
import expr.Expr
import lex.SubExpr
import parser.Parser.Result

import scala.annotation.tailrec

trait Parser {
  def parse(tokens: List[SubExpr]): Parser.Result[Expr]
}

object DefaultParser extends Parser {

  private object ContainsFunction {
    private def containsFunctionAt(tokens: List[SubExpr]): Option[Int] =
      Option(tokens.indexWhere {
        case SubExpr.F(_) => true
        case _ => false
      }).filter(_ >= 0)

    def unapply(tokens: List[SubExpr]): Option[(List[SubExpr], String, Expr, List[SubExpr])] = {
      containsFunctionAt(tokens).map { functionIndex =>
        val (heads, tailStartingWithFunction) = tokens.splitAt(functionIndex)
        tailStartingWithFunction match {
          case SubExpr.F(name) :: SubExpr.`(` :: SubExpr.S(arg) :: SubExpr.`)` :: rest =>
            (heads, name, arg, rest)
          case _ =>
            throw ParseError.InvalidInputSyntax
        }
      }
    }
  }

  private object `Has ( S )` {

    @tailrec
    private def indexWhereSBetweenParenthesis(tokens: List[SubExpr], currIndex: Int = 0): Option[Int] = tokens match {
      case list if list.sizeIs < 3 => None
      case SubExpr.`(` :: SubExpr.S(_) :: SubExpr.`)` :: _ => Some(currIndex)
      case _ :: tail => indexWhereSBetweenParenthesis(tail, currIndex + 1)
    }

    def unapply(tokens: List[SubExpr]): Option[(List[SubExpr], SubExpr, List[SubExpr])] = {
      indexWhereSBetweenParenthesis(tokens).map { index =>
        val (before, SubExpr.`(` :: SubExpr.S(e) :: SubExpr.`)` :: rest) = tokens.splitAt(index): @unchecked
        (before, SubExpr.S(e), rest)
      }
    }
  }

  private object HasExponentiation {

    private def lastExponentiationIndex(tokens: List[SubExpr]): Option[Int] = {
      @tailrec
      def firstExponentiationIndex(tokens: List[SubExpr], currIndex: Int = 0): Option[Int] = tokens match {
        case list if list.sizeIs < 3 => None
        case SubExpr.S(_) :: SubExpr.`^` :: SubExpr.S(_) :: _ => Some(currIndex)
        case _ :: tail => firstExponentiationIndex(tail, currIndex + 1)
      }

      firstExponentiationIndex(tokens.reverse).map(index => tokens.length - index - 2)
    }

    def unapply(tokens: List[SubExpr]): Option[(List[SubExpr], Expr, Expr , List[SubExpr])] = {
      lastExponentiationIndex(tokens).flatMap { index =>
        val (beginning, restStartingWithBase) = tokens.splitAt(index - 1)
        restStartingWithBase match {
          case SubExpr.S(base) :: SubExpr.`^` :: SubExpr.S(exponent) :: rest => Some((beginning, base, exponent, rest))
          case _ => None
        }
      }
    }
  }

  private object HasMultiplication {

    @tailrec
    private def indexWhereSMultipliedByS(tokens: List[SubExpr], currIndex: Int = 0): Option[Int] = tokens match {
      case list if list.sizeIs < 3 => None
      case SubExpr.S(_) :: SubExpr.`*` :: SubExpr.S(_) :: _ => Some(currIndex)
      case _ :: tail => indexWhereSMultipliedByS(tail, currIndex + 1)
    }

    def unapply(tokens: List[SubExpr]): Option[(List[SubExpr], Expr, Expr, List[SubExpr])] = {
      indexWhereSMultipliedByS(tokens).map { index =>
        val (beginning, SubExpr.S(lhs) :: _ :: SubExpr.S(rhs) :: rest) = tokens.splitAt(index): @unchecked
        (beginning, lhs, rhs, rest)
      }
    }
  }

  private object HasDivision {
    @tailrec
    private def indexWhereSDividedByS(tokens: List[SubExpr], currIndex: Int = 0): Option[Int] = tokens match {
      case list if list.sizeIs < 3 => None
      case SubExpr.S(_) :: SubExpr.`/` :: SubExpr.S(_) :: _ => Some(currIndex)
      case _ :: tail => indexWhereSDividedByS(tail, currIndex + 1)
    }

    def unapply(tokens: List[SubExpr]): Option[(List[SubExpr], Expr, Expr, List[SubExpr])] = {
      indexWhereSDividedByS(tokens).map { index =>
        val (beginning, SubExpr.S(lhs) :: _ :: SubExpr.S(rhs) :: rest) = tokens.splitAt(index): @unchecked
        (beginning, lhs, rhs, rest)
      }
    }
  }

  private object HasAddition {
    @tailrec
    private def indexWhereSAddedToS(tokens: List[SubExpr], currIndex: Int = 0): Option[Int] = tokens match {
      case list if list.sizeIs < 3 => None
      case SubExpr.S(_) :: SubExpr.`+` :: SubExpr.S(_) :: _ => Some(currIndex)
      case _ :: tail => indexWhereSAddedToS(tail, currIndex + 1)
    }

    def unapply(tokens: List[SubExpr]): Option[(List[SubExpr], Expr, Expr, List[SubExpr])] = {
      indexWhereSAddedToS(tokens).map { index =>
        val (beginning, SubExpr.S(lhs) :: _ :: SubExpr.S(rhs) :: rest) = tokens.splitAt(index): @unchecked
        (beginning, lhs, rhs, rest)
      }
    }
  }

  private object HasSubtraction {
    @tailrec
    private def indexWhereSSubtractedFromS(tokens: List[SubExpr], currIndex: Int = 0): Option[Int] = tokens match {
      case list if list.sizeIs < 3 => None
      case SubExpr.S(_) :: SubExpr.`-` :: SubExpr.S(_) :: _ => Some(currIndex)
      case _ :: tail => indexWhereSSubtractedFromS(tail, currIndex + 1)
    }

    def unapply(tokens: List[SubExpr]): Option[(List[SubExpr], Expr, Expr, List[SubExpr])] = {
      indexWhereSSubtractedFromS(tokens).map { index =>
        val (beginning, SubExpr.S(lhs) :: _ :: SubExpr.S(rhs) :: rest) = tokens.splitAt(index): @unchecked
        (beginning, lhs, rhs, rest)
      }
    }
  }

  private object HasNeg {
    @tailrec
    private def indexWhereSIsNegated(tokens: List[SubExpr], currIndex: Int = 0): Option[Int] = tokens match {
      case list if list.sizeIs < 2 => None
      case SubExpr.S(_) :: SubExpr.`-` :: SubExpr.S(_) :: _ => Some(currIndex)
      case _ :: tail => indexWhereSIsNegated(tail, currIndex + 1)
    }
  }

  @tailrec
  override def parse(tokens: List[SubExpr]): Result[Expr] = {
    tokens match {
      case SubExpr.S(exp) :: Nil => Right(exp)
      case ContainsFunction(beginning, funcName, arg, rest) =>
        parse(beginning ::: (SubExpr.S(Expr.Func(funcName, arg)) :: rest))
      case `Has ( S )`(beginning, s, rest) =>
        parse(beginning ::: (s :: rest))
      case HasExponentiation(beginning, base, exponent, rest) =>
        parse(beginning ::: (SubExpr.S(Expr.Pow(base, exponent)) :: rest))
      case HasMultiplication(beginning, lhs, rhs, rest) =>
        parse(beginning ::: (SubExpr.S(Expr.Mul(lhs, rhs)) :: rest))
      case HasDivision(beginning, lhs, rhs, rest) =>
        parse(beginning ::: (SubExpr.S(Expr.Div(lhs, rhs)) :: rest))
      case HasAddition(beginning, lhs, rhs, rest) =>
        parse(beginning ::: (SubExpr.S(Expr.Add(lhs, rhs)) :: rest))
      case HasSubtraction(beginning, lhs, rhs, rest) =>
        parse(beginning ::: (SubExpr.S(Expr.Sub(lhs, rhs)) :: rest))
      case other => Left(ParseError.UnrecognizedPattern(other.mkString("[", ",", "]")))
    }
  }
}

object Parser {
  type Result[T] = Either[ParseError, T]
}
