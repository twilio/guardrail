package com.twilio.guardrail.protocol.terms.protocol

sealed trait PropertyRequirement

/**
  * Types that are represented as Option[T]
  */
object PropertyRequirement {
  case object Required         extends PropertyRequirement
  case object OptionalNullable extends PropertyRequirement

  sealed trait OptionalRequirement extends PropertyRequirement

  case object RequiredNullable extends OptionalRequirement
  case object Optional         extends OptionalRequirement
  case object OptionalLegacy   extends OptionalRequirement

  final case class Configured(encoder: OptionalRequirement, decoder: OptionalRequirement) extends PropertyRequirement
}

sealed trait PropertyConstraint
object PropertyConstraint {
  case class Maximum(value: BigDecimal) extends PropertyConstraint
  case class Minimum(value: BigDecimal) extends PropertyConstraint
}
