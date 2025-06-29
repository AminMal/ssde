package errors

enum EvaluationError(m: String) extends RuntimeException {
  case VariableNotFound(varName: String) extends EvaluationError(s"variable definition not found: $varName")

  override def getMessage: String = m
}