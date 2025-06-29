package errors

enum ParseError(m: String) extends RuntimeException {
  case InvalidInputSyntax extends ParseError("invalid input syntax")
  case InvalidOperatorPosition(op: String) extends ParseError(s"invalid operator position [$op]")
  case UnrecognizedPattern(p: String) extends ParseError(s"unrecognized pattern: '$p'")

  override def getMessage: String = m
}