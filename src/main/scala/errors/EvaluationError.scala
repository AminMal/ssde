package errors

enum EvaluationError(m: String) extends RuntimeException {
  case VariableNotFound(varName: Char) extends EvaluationError(s"variable definition not found: $varName")

  override def getMessage: String = m
}