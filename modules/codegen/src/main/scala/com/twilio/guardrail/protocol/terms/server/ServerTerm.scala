package com.twilio.guardrail.protocol.terms.server

import com.twilio.guardrail.generators.ScalaParameters
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.protocol.terms.Responses
import com.twilio.guardrail.terms.{ RouteMeta, SecurityScheme }
import com.twilio.guardrail.{ RenderedRoutes, StrictProtocolElems, SupportDefinition, TracingField }
import io.swagger.v3.oas.models.Operation

sealed trait ServerTerm[L <: LA, T]
case class BuildTracingFields[L <: LA](operation: Operation, resourceName: List[String], tracing: Boolean) extends ServerTerm[L, Option[TracingField[L]]]
case class GenerateRoutes[L <: LA](tracing: Boolean,
                                   resourceName: String,
                                   basePath: Option[String],
                                   routes: List[(String, Option[TracingField[L]], RouteMeta, ScalaParameters[L], Responses[L])],
                                   protocolElems: List[StrictProtocolElems[L]],
                                   securitySchemes: Map[String, SecurityScheme[L]],
                                   authedRoutes: Boolean)
    extends ServerTerm[L, RenderedRoutes[L]]
case class GetExtraRouteParams[L <: LA](tracing: Boolean) extends ServerTerm[L, List[L#MethodParameter]]
case class GenerateResponseDefinitions[L <: LA](operationId: String, responses: Responses[L], protocolElems: List[StrictProtocolElems[L]])
    extends ServerTerm[L, List[L#Definition]]
case class GenerateSupportDefinitions[L <: LA](tracing: Boolean, securitySchemes: Map[String, SecurityScheme[L]])
    extends ServerTerm[L, List[SupportDefinition[L]]]
case class RenderClass[L <: LA](className: String,
                                handlerName: String,
                                annotations: List[L#Annotation],
                                routeTerms: List[L#Term],
                                secureRouteTerms: List[L#Term],
                                extraRouteParams: List[L#MethodParameter],
                                responseDefinitions: List[L#Definition],
                                supportDefinitions: List[L#Definition])
    extends ServerTerm[L, List[L#Definition]]
case class RenderHandler[L <: LA](handlerName: String,
                                  methodSigs: List[L#MethodDeclaration],
                                  handlerDefinitions: List[L#Statement],
                                  responseDefinitions: List[L#Definition],
                                  securityRequirements: Boolean)
    extends ServerTerm[L, L#Definition]
case class GetExtraImports[L <: LA](tracing: Boolean) extends ServerTerm[L, List[L#Import]]
