package com.twilio.guardrail

case class Context(framework: Option[String], tracing: Boolean, http4sAuthedRoutes: Boolean)

object Context {
  val empty = Context(None, tracing = false, http4sAuthedRoutes = false)
}
