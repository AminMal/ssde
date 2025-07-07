import expr.Expr
import lex.Lexer
import parser.DefaultParser
import expr.Syntax.{*, given}

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

object Main extends App {

  val lexer = new Lexer()
  def parse[U](s: String)(f: Expr => U): Unit = {
    val tokens = lexer.tokenize(s)
    DefaultParser.parse(tokens) match {
      case Left(error) => error.printStackTrace()
      case Right(exp) => f(exp)
    }
  }

  val start = LocalDateTime.now()

  parse("2^10(x)^2 / 3^4x + 2x") { expression =>
    val der = expression.derivativeOver(x)
    println(s"value at point x = 1 is ${der.solveFor('x' -> 1)}")
  }

  val end = LocalDateTime.now()
  val dur = ChronoUnit.MICROS.between(start, end)
  println(s"It took ${dur}µs to complete")
}
