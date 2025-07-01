package lex

import expr.Expr
import SubExpr.*

import scala.annotation.tailrec

class Lexer {
  
  private object Letter {
    def unapply(token: Char): Option[Char] = if token.isLetter then Some(token) else None
  }
  
  @tailrec
  private def tokenizeInternal(in: List[Char], acc: List[SubExpr]): List[SubExpr] = in match {
    case Nil => acc.reverse
    case '^' :: rest => tokenizeInternal(rest, `^` :: acc)
    case '*' :: rest => tokenizeInternal(rest, `*` :: acc)
    case '+' :: rest => tokenizeInternal(rest, `+` :: acc)
    case '-' :: rest => tokenizeInternal(rest, `-` :: acc)
    case '(' :: rest => tokenizeInternal(rest, `(` :: acc)
    case ')' :: rest => tokenizeInternal(rest, `)` :: acc)
    case '/' :: rest => tokenizeInternal(rest, `/` :: acc)
    // Numbers
    case c :: _ if c.isDigit =>
      val (digits, rest) = in.span(_.isDigit)
      val s = S(Expr.Const(digits.mkString.toInt)) // TODO: this is unsafe, fix later
      val restRefined = rest.headOption match {
        case Some(c) if c.isLetter || c == '(' => '*' :: rest
        case _ => rest
      }
      tokenizeInternal(restRefined, s :: acc)
    // Vars and e 
    case Letter(x) :: rest if !rest.headOption.exists(_.isLetter) && !rest.headOption.contains('(') =>
      x match {
        case 'e' => tokenizeInternal(rest, S(Expr.e) :: acc)
        case _   => tokenizeInternal(rest, S(Expr.Var(x.toString)) :: acc)
      }
    // Function symbol
    case a :: _ if a.isLetter =>
      val (letters, rest) = in.span(_.isLetter)
      val f = F(letters.mkString)
      tokenizeInternal(rest, f :: acc)
    case other :: rest =>
      println(s"[WARN] Skipping unrecognized character [$other]")
      tokenizeInternal(rest, acc)
  }

  private def trimSpaces(s: String): List[Char] =
    s
      .replaceAll("\\s+", "")
      .replaceAll("\\t+", "")
      .replaceAll("\\n+", "")
      .toList

  def tokenize(in: String): List[SubExpr] = tokenizeInternal(trimSpaces(in), Nil)
}
