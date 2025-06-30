package lex

import expr.Expr
import SubExpr.*
import org.scalatest.*
import flatspec.*
import matchers.*

class LexerTest extends AnyFlatSpecLike with should.Matchers {
  val lexer = new Lexer()

  private final val x = S(Expr.Var("x"))
  private final val y = S(Expr.Var("y"))

  "Lexer" should "extract proper tokens for simple constant number" in {
    val expr = "2"
    lexer.tokenize(expr) shouldEqual List(S(Expr.Const(2)))
  }

  "Lexer" should "differentiate function application with variable name" in {
    val expr = "x + fib(x + 1) + otherfunc(3)"
    val tokens = lexer.tokenize(expr)
    val expected = List(x, `+`, F("fib"), `(`, x, `+`, S(Expr.Const(1)), `)`, `+`, F("otherfunc"), `(`, S(Expr.Const(3)), `)`)
    tokens shouldEqual expected
  }

  "Lexer" should "not consider `e` as variable" in {
    val expr = "e"
    val tokens = lexer.tokenize(expr)
    val expected = List(S(Expr.e))
    tokens shouldEqual expected
  }

  "Lexer" should "differentiate `e`, vars and functions" in {
    val expr = "x + e + fib(x - y + 1)"

    val tokens = lexer.tokenize(expr)
    val expected = List(x, `+`, S(Expr.e), `+`, F("fib"), `(`, x, `-`, y, `+`, S(Expr.Const(1)), `)`)
    tokens shouldEqual expected
  }

  "Lexer" should "be case insensitive" in {
    val expr1 = "2 ^ 30 (x + 3) ^ 2"
    val expr2 = "2^30(x+3)^2"
    lexer.tokenize(expr1) shouldEqual lexer.tokenize(expr2)
  }

  "Lexer" should "auto-place multiplication between numbers and following variable" in {
    val expr = "fib(5) + 6x + 2"
    val tokens = lexer.tokenize(expr)
    tokens shouldEqual List(F("fib"), `(`, S(Expr.Const(5)), `)`, `+`, S(Expr.Const(6)), `*`, x, `+`, S(Expr.Const(2)))
  }

  "Lexer" should "auto-place multiplication between numbers and following function invocations" in {
    val expr = "6fib(5) + x + 2"
    val tokens = lexer.tokenize(expr)
    tokens shouldEqual List(S(Expr.Const(6)), `*`, F("fib"), `(`, S(Expr.Const(5)), `)`, `+`, x, `+`, S(Expr.Const(2)))
  }

  "Lexer" should "auto-place multiplication between numbers and following open parenthesis" in {
    val expr = "6(5) + x - 2"
    val tokens = lexer.tokenize(expr)
    tokens shouldEqual List(S(Expr.Const(6)), `*`, `(`, S(Expr.Const(5)), `)`, `+`, x, `-`, S(Expr.Const(2)))
  }

  "Lexer" should "detect subtraction" in {
    val expr = "a - b + 2"
    val tokens = lexer.tokenize(expr)
    tokens shouldEqual List(S(Expr.Var("a")), `-`, S(Expr.Var("b")), `+`, S(Expr.Const(2)))
  }

  "Lexer" should "detect parenthesis" in {
    val expr = "(2)"
    val tokens = lexer.tokenize(expr)

    tokens shouldEqual List(`(`, S(Expr.Const(2)), `)`)
  }

  "Lexer" should "not be concerned with parenthesis alignments" in {
    val expr = "x + (2 - 1))("
    val tokens = lexer.tokenize(expr)

    tokens shouldEqual List(x, `+`, `(`, S(Expr.Const(2)), `-`, S(Expr.Const(1)), `)`, `)`, `(`)
  }
}
