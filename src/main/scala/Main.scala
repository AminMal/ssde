import expr.{Expr, Simplifier}
import lex.Lexer
import parser.DefaultParser
import expr.Syntax.{*, given}

object Main extends App {

  val lexer = new Lexer()
  def parse[U](s: String)(f: Expr => U): Unit = {
    val tokens = lexer.tokenize(s)
    DefaultParser.parse(tokens) match {
      case Left(error) => error.printStackTrace()
      case Right(exp) => f(exp)
    }
  }

  parse("2^30(x)^2 - sin(x)*3^4") { expression =>
    println(s"expression is ${expression.simplified.show}")
    val derivative = expression.derivativeOver(x)
    println(s"derivative is ${derivative.show}")
    println(s"derivative is ${Simplifier.simplify(derivative).show}")
    println(s"derivative at point x = 1 is ${derivative.solveFor("x" -> 1)}")
  }
}
