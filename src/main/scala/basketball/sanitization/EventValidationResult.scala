package basketball.sanitization

object EventValidationResult extends Enumeration {
  val VALID, ONE_MISSING, ONE_NUMBER_MALFORMED, INVALID = Value
}
