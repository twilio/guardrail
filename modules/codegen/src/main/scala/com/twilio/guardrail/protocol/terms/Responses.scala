package com.twilio.guardrail.protocol.terms

import cats.free.Free
import cats.implicits._
import com.twilio.guardrail.{ StrictProtocolElems, SwaggerUtil }
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.generators.syntax._
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.terms.{ ScalaTerms, SwaggerTerms }
import com.twilio.guardrail.terms.framework.FrameworkTerms
import io.swagger.v3.oas.models.Operation

class Response[L <: LA](val statusCodeName: L#TermName, val statusCode: Int, val value: Option[(L#Type, Option[L#Term])], val headers: Headers[L]) {
  override def toString: String = s"Response($statusCodeName, $statusCode, $value, $headers)"
}
object Response {
  def unapply[L <: LA](value: Response[L]): Option[(L#TermName, Option[L#Type], Headers[L])] =
    Some((value.statusCodeName, value.value.map(_._1), value.headers))
}

class Responses[L <: LA](val value: List[Response[L]]) {
  override def toString: String = s"Responses($value)"
}
object Responses {
  def getResponses[L <: LA, F[_]](operationId: String, operation: Tracker[Operation], protocolElems: List[StrictProtocolElems[L]])(
      implicit Fw: FrameworkTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, Responses[L]] = Sw.log.function("getResponses") {
    import Fw._
    import Sc._
    for {
      responses <- Sw.getResponses(operationId, operation)

      instances <- responses.toList.flatTraverse {
        case (key, resp) =>
          if (key == "default") {
            Sw.log.warning(s"Ignoring 'default' response code at ${resp.showHistory}").map { _ =>
              List.empty[Response[L]]
            }
          } else {
            for {
              httpCode <- lookupStatusCode(key)
              (statusCode, statusCodeName) = httpCode
              valueTypes <- (for {
                (_, content) <- resp.downField("content", _.getContent()).indexedDistribute.value
                schema       <- content.downField("schema", _.getSchema()).indexedDistribute.toList
              } yield schema).traverse { prop =>
                for {
                  meta     <- SwaggerUtil.propMeta[L, F](prop)
                  resolved <- SwaggerUtil.ResolvedType.resolve[L, F](meta, protocolElems)
                  SwaggerUtil.Resolved(baseType, _, baseDefaultValue, _, _) = resolved

                } yield (baseType, baseDefaultValue)
              }
              headers <- resp.downField("headers", _.getHeaders).indexedDistribute.value.traverse {
                case (name, header) =>
                  for {
                    termName <- pureTermName(s"${name}Header".toCamelCase)
                    typeName <- pureTypeName("String").flatMap(widenTypeName)
                    required = header.downField("required", _.getRequired).unwrapTracker.getOrElse(false)
                    resultType <- if (required) Free.pure[F, L#Type](typeName) else liftOptionalType(typeName)
                  } yield new Header(name, required, resultType, termName)
              }
            } yield List(new Response[L](statusCodeName, statusCode, valueTypes.headOption, new Headers(headers))) // FIXME: headOption
          }
      }
    } yield new Responses[L](instances)
  }

}
