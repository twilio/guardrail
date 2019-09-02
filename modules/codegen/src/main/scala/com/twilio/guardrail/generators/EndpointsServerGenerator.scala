package com.twilio.guardrail
package generators

import cats.arrow.FunctionK
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.protocol.terms.server._

object EndpointsServerGenerator {
  object ServerTermInterp extends FunctionK[ServerTerm[ScalaLanguage, ?], Target] {
    def apply[T](term: ServerTerm[ScalaLanguage, T]): Target[T] = term match {
      case GenerateResponseDefinitions(operationId, responses, protocolElems)                                                                           => ???
      case BuildTracingFields(operation, resourceName, tracing)                                                                                         => ???
      case GenerateRoutes(tracing, resourceName, basePath, routes, protocolElems, securitySchemes, authedRoutes)                                        => ???
      case RenderHandler(handlerName, methodSigs, handlerDefinitions, responseDefinitions, securityRequirements)                                        => ???
      case GetExtraRouteParams(tracing)                                                                                                                 => ???
      case GenerateSupportDefinitions(tracing, securitySchemes)                                                                                         => ???
      case RenderClass(resourceName, handlerName, annotations, routeTerms, secureRouteTerms, extraRouteParams, responseDefinitions, supportDefinitions) => ???
      case GetExtraImports(tracing)                                                                                                                     => ???
    }
  }
}
