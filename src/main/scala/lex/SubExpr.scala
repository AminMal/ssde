package lex

import expr.Expr

enum SubExpr {
  case S(e: Expr)
  case F(func: String)
  case `^`
  case `*`
  case `/`
  case `+`
  case `-`
  case `(`
  case `)`
  case App(func: String, arg: Expr)
}