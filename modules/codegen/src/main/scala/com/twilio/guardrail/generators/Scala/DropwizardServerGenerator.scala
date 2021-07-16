package com.twilio.guardrail.generators.Scala

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.all._
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.generators.{ LanguageParameter, RawParameterName }
import com.twilio.guardrail.generators.helpers.DropwizardHelpers._
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.protocol.terms.server.{ GenerateRouteMeta, ServerTerms }
import com.twilio.guardrail.protocol.terms._
import com.twilio.guardrail.shims.OperationExt
import com.twilio.guardrail.terms.{ CollectionsLibTerms, RouteMeta, SecurityScheme }
import com.twilio.guardrail.{ CustomExtractionField, RenderedRoutes, StrictProtocolElems, SupportDefinition, Target, TracingField }
import io.swagger.v3.oas.models.Operation

import scala.meta._

object DropwizardServerGenerator {
  val buildTermSelect: NonEmptyList[String] => Term.Ref = {
    case NonEmptyList(head, tail) =>
      tail.map(Term.Name.apply _).foldLeft[Term.Ref](Term.Name(head))(Term.Select.apply _)
  }

  private val PLAIN_TYPES =
    Set("Boolean", "Byte", "Char", "Short", "Int", "Long", "BigInt", "Float", "Double", "BigDecimal", "String", "OffsetDateTime", "LocalDateTime")
  private val CONTAINER_TYPES = Seq("Vector", "List", "Seq", "IndexedSeq", "Iterable", "Map")

  private implicit class ContentTypeExt(private val ct: ContentType) extends AnyVal {
    def toJaxRsAnnotationName: Term = ct match {
      case ApplicationJson     => q"MediaType.APPLICATION_JSON"
      case UrlencodedFormData  => q"MediaType.APPLICATION_FORM_URLENCODED"
      case MultipartFormData   => q"MediaType.MULTIPART_FORM_DATA"
      case TextPlain           => q"MediaType.TEXT_PLAIN"
      case OctetStream         => q"MediaType.APPLICATION_OCTET_STREAM"
      case TextContent(name)   => Lit.String(name)
      case BinaryContent(name) => Lit.String(name)
    }
  }

  def unwrapContainer(tpe: Type): (Type, Type => Type) = {
    def rewrap(wrapper: Type)(tpe: Type): Type = Type.Apply(wrapper, List(tpe))
    tpe match {
      case t"Option[$inner]"       => (inner, rewrap(t"Option"))
      case t"Vector[$inner]"       => (inner, rewrap(t"Vector"))
      case t"List[$inner]"         => (inner, rewrap(t"List"))
      case t"Seq[$inner]"          => (inner, rewrap(t"Seq"))
      case t"IndexedSeq[$inner]"   => (inner, rewrap(t"IndexedSeq"))
      case t"Iterable[$inner]"     => (inner, rewrap(t"Iterable"))
      case t"Map[$keyTpe, $inner]" => (inner, x => Type.Apply(t"Map", List(keyTpe, x)))
      case other                   => (other, identity)
    }
  }

  private object paramTransformers {
    private def annotateHttpParameter(parameterName: RawParameterName, annotationName: Option[String])(param: Term.Param): Term.Param = param.copy(
      mods = annotationName.map(an => Mod.Annot(Init(Type.Name(an), Name.Anonymous(), List(List(Lit.String(parameterName.value)))))).toList ++ param.mods
    )

    private def handleDefaultValue(defaultValue: Option[Term])(param: Term.Param): Term.Param = param.copy(
      mods = defaultValue
          .flatMap({ dv =>
            val defaultStr = dv match {
              case s @ Lit.String(_) => Some(s)
              case Lit.Boolean(b)    => Some(Lit.String(b.toString))
              case Lit.Byte(b)       => Some(Lit.String(b.toString))
              case Lit.Char(c)       => Some(Lit.String(c.toString))
              case Lit.Short(s)      => Some(Lit.String(s.toString))
              case Lit.Int(i)        => Some(Lit.String(i.toString))
              case Lit.Long(l)       => Some(Lit.String(l.toString))
              case Lit.Float(f)      => Some(Lit.String(f))
              case Lit.Double(d)     => Some(Lit.String(d))
              case _                 => None
            }
            defaultStr.map(s => Mod.Annot(Init(Type.Name("Default"), Name.Anonymous(), List(List(s)))))
          })
          .toList ++ param.mods,
      default = None
    )

    private def addValidation(param: Term.Param): Term.Param = param.copy(
      mods = List(mod"@(NotNull @param @field)") ++
            param.decltpe
              .flatMap({
                case Type.Select(Term.Select(q"java", q"time"), Type.Name(_))                     => Some(mod"@Unwrapping")
                case Type.Select(Term.Select(q"GuardrailJerseySupport", q"Jsr310"), Type.Name(_)) => Some(mod"@Unwrapping")
                case _                                                                            => None
              })
              .toList ++ param.mods
    )

    private def transformJsr310Param(param: Term.Param): Term.Param = param.copy(
      decltpe = param.decltpe.map({ tpe =>
        val (unwrapped, rewrap) = unwrapContainer(tpe)
        rewrap(unwrapped match {
          case Type.Select(Term.Select(q"java", q"time"), Type.Name(jsr310Type)) =>
            Type.Select(Term.Select(q"GuardrailJerseySupport", q"Jsr310"), Type.Name(s"${jsr310Type}Param"))
          case decltpe => decltpe
        })
      })
    )

    def stripOptionFromCollections(param: Term.Param): Term.Param = param.copy(
      decltpe = param.decltpe.map({
        case Type.Apply(t"Option", List(Type.Apply(containerType, innerTypes))) if CONTAINER_TYPES.contains(containerType.toString) =>
          t"$containerType[..$innerTypes]"
        case other => other
      })
    )

    def replaceFileParam(in: Option[String], isFile: Boolean)(param: Term.Param): Term.Param =
      if (!in.contains("body") && isFile)
        param.copy(
          decltpe = param.decltpe.map({ tpe =>
            val (unwrapped, rewrap) = unwrapContainer(tpe)
            unwrapped match {
              case t"java.io.InputStream" => rewrap(t"java.io.File")
              case other                  => rewrap(other)
            }
          })
        )
      else param

    // Transformers are listed in reverse order.  The order does matter, for at least
    // one reason: https://github.com/eclipse-ee4j/jersey/issues/3632
    private def buildTransformers(param: LanguageParameter[ScalaLanguage], httpParameterAnnotation: Option[String]): List[Term.Param => Term.Param] = List(
      replaceFileParam(param.in, param.isFile),
      stripOptionFromCollections,
      transformJsr310Param,
      handleDefaultValue(param.param.default),
      annotateHttpParameter(param.argName, httpParameterAnnotation),
      addValidation
    )

    def transform(param: LanguageParameter[ScalaLanguage], httpParameterAnnotation: Option[String]): Term.Param =
      buildTransformers(param, httpParameterAnnotation).foldLeft(param.param)((accum, next) => next(accum))
  }

  def ServerTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): ServerTerms[ScalaLanguage, Target] =
    new ServerTermInterp
  class ServerTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]) extends ServerTerms[ScalaLanguage, Target] {
    override def MonadF: Monad[Target] = Target.targetInstances

    override def getExtraImports(tracing: Boolean, supportPackage: NonEmptyList[String]): Target[List[Import]] =
      Target.pure(
        List(
          q"import io.dropwizard.jersey.PATCH",
          q"import javax.validation.constraints.NotNull",
          q"import javax.ws.rs.container.{AsyncResponse, Suspended}",
          q"import javax.ws.rs.core.Response.Status",
          q"import javax.ws.rs.core.{MediaType, Response}",
          q"import javax.ws.rs.{Consumes, DELETE, FormParam, GET, HEAD, HeaderParam, OPTIONS, POST, PUT, Path, PathParam, Produces, QueryParam}",
          q"import org.glassfish.jersey.media.multipart.FormDataParam",
          q"import javax.validation.valueextraction.Unwrapping",
          q"import org.slf4j.LoggerFactory",
          q"import scala.annotation.meta.{field, param}",
          q"import scala.concurrent.{ExecutionContext, Future}",
          q"import scala.util.{Failure, Success}",
          q"import ${buildTermSelect(supportPackage)}.GuardrailJerseySupport"
        )
      )

    override def generateSupportDefinitions(
        tracing: Boolean,
        securitySchemes: Map[String, SecurityScheme[ScalaLanguage]]
    ): Target[List[SupportDefinition[ScalaLanguage]]] =
      Target.pure(
        List(
          SupportDefinition[ScalaLanguage](
            q"GuardrailJerseySupport",
            List(
              q"import io.dropwizard.jersey.params.AbstractParam",
              q"import java.time._",
              q"import java.util.Objects",
              q"import javax.ws.rs.BadRequestException",
              q"import scala.util.Try"
            ),
            List(
              q"""
              object GuardrailJerseySupport {
                object Jsr310 {
                  abstract class GuardrailAbstractParam[T <: AnyRef] protected (input: String, parameterName: String) extends AbstractParam[T](input, parameterName) {
                    def this(input: String) = this(input, "Parameter")

                    private val value: T = Try(realParse(input)).getOrElse({
                      throw new BadRequestException(s"$$parameterName is invalid: $$input")
                    })

                    override protected def parse(input: String): T = null.asInstanceOf[T]
                    def realParse(input: String): T
                    override def get(): T = this.value

                    override def equals(other: Any): Boolean = other match {
                      case that: GuardrailAbstractParam[_] => value == that.value
                      case _ => false
                    }

                    override def hashCode(): Int = Objects.hash(value)

                    override def toString = s"$${getClass.getSimpleName}(value=$$value)"
                  }

                  class InstantParam(input: String, parameterName: String) extends GuardrailAbstractParam[Instant](input, parameterName) {
                    override def realParse(input: String): Instant = Instant.parse(input)
                  }
                  class OffsetDateTimeParam(input: String, parameterName: String) extends GuardrailAbstractParam[OffsetDateTime](input, parameterName) {
                    override def realParse(input: String): OffsetDateTime = OffsetDateTime.parse(input)
                  }
                  class ZonedDateTimeParam(input: String, parameterName: String) extends GuardrailAbstractParam[ZonedDateTime](input, parameterName) {
                    override def realParse(input: String): ZonedDateTime = ZonedDateTime.parse(input)
                  }
                  class LocalDateTimeParam(input: String, parameterName: String) extends GuardrailAbstractParam[LocalDateTime](input, parameterName) {
                    override def realParse(input: String): LocalDateTime = LocalDateTime.parse(input)
                  }
                  class LocalDateParam(input: String, parameterName: String) extends GuardrailAbstractParam[LocalDate](input, parameterName) {
                    override def realParse(input: String): LocalDate = LocalDate.parse(input)
                  }
                  class OffsetTimeParam(input: String, parameterName: String) extends GuardrailAbstractParam[OffsetTime](input, parameterName) {
                    override def realParse(input: String): OffsetTime = OffsetTime.parse(input)
                  }
                  class LocalTimeParam(input: String, parameterName: String) extends GuardrailAbstractParam[LocalTime](input, parameterName) {
                    override def realParse(input: String): LocalTime = LocalTime.parse(input)
                  }
                  class DurationParam(input: String, parameterName: String) extends GuardrailAbstractParam[Duration](input, parameterName) {
                    override def realParse(input: String): Duration = Duration.parse(input)
                  }
                }
              }
             """
            ),
            insideDefinitions = false
          )
        )
      )

    override def buildCustomExtractionFields(
        operation: Tracker[Operation],
        resourceName: List[String],
        customExtraction: Boolean
    ): Target[Option[CustomExtractionField[ScalaLanguage]]] =
      if (customExtraction) {
        Target.raiseUserError(s"Custom Extraction is not yet supported by this framework")
      } else {
        Target.pure(Option.empty)
      }

    override def buildTracingFields(operation: Tracker[Operation], resourceName: List[String], tracing: Boolean): Target[Option[TracingField[ScalaLanguage]]] =
      Target.pure(None)

    override def generateResponseDefinitions(
        responseClsName: String,
        responses: Responses[ScalaLanguage],
        protocolElems: List[StrictProtocolElems[ScalaLanguage]]
    ): Target[List[Defn]] = {
      val responseClsType = Type.Name(responseClsName)
      val responseInstances: List[(Defn, Defn)] = responses.value.map({ response =>
        val responseClsSubType = Type.Name(s"$responseClsName${response.statusCodeName}")
        val responseClsSubTerm = Term.Name(s"$responseClsName${response.statusCodeName}")
        val statusCodeTerm     = Lit.Int(response.statusCode)
        response.value.fold[(Defn, Defn)](
          (
            q"case object $responseClsSubTerm extends $responseClsType($statusCodeTerm)",
            q"def ${response.statusCodeName}: $responseClsType = $responseClsSubTerm"
          )
        )({
          case (contentType, valueType, _) =>
            (
              q"case class $responseClsSubType(value: $valueType) extends $responseClsType($statusCodeTerm)",
              q"def ${response.statusCodeName}(value: $valueType): $responseClsType = $responseClsSubTerm(value)"
            )
        })
      })
      Target.pure(
        List(
          q"""sealed abstract class ${Type.Name(responseClsName)}(val statusCode: Int) extends Product with Serializable""",
          q"""
              object ${Term.Name(responseClsName)} {
                ..${responseInstances.map(_._2)}
              }
           """
        ) ++ responseInstances.map(_._1)
      )
    }

    override def generateRoutes(
        tracing: Boolean,
        resourceName: String,
        handlerName: String,
        basePath: Option[String],
        routes: List[GenerateRouteMeta[ScalaLanguage]],
        protocolElems: List[StrictProtocolElems[ScalaLanguage]],
        securitySchemes: Map[String, SecurityScheme[ScalaLanguage]]
    ): Target[RenderedRoutes[ScalaLanguage]] = {
      val basePathComponents = basePath.toList.flatMap(splitPathComponents)
      val commonPathPrefix   = findPathPrefix(routes.map(_.routeMeta.path.unwrapTracker))
      val fullPathPrefix     = (basePathComponents ++ commonPathPrefix).mkString("/", "/", "")

      val resourceNameTerm = Term.Name(resourceName)

      val (routeMethods, handlerMethodSigs) = routes
        .map({
          case GenerateRouteMeta(
              operationId,
              methodName,
              responseClsName,
              customExtractionFields,
              _,
              RouteMeta(path, method, operation, _),
              parameters,
              responses
              ) =>
            val pathSuffix     = splitPathComponents(path.unwrapTracker).drop(commonPathPrefix.length).mkString("/", "/", "")
            val pathAnnotation = Option(pathSuffix).filter(_.nonEmpty).filterNot(_ == "/").map(p => mod"@Path(${Lit.String(p)})")

            val httpMethodAnnotation = Mod.Annot(Init(Type.Name(method.name()), Name.Anonymous(), List.empty))

            val allConsumes        = operation.downField("consumes", _.consumes).map(_.flatMap(ContentType.unapply)).unwrapTracker
            val consumes           = getBestConsumes(operation, allConsumes, parameters)
            val consumesAnnotation = consumes.map(c => mod"@Consumes(Array(..${List(c.toJaxRsAnnotationName)}))")

            def isTypePlain(tpe: Type): Boolean =
              tpe match {
                case Type.Name(name) if PLAIN_TYPES.contains(name)                 => true
                case Type.Select(_, Type.Name(name)) if PLAIN_TYPES.contains(name) => true
                case _                                                             => false
              }

            val allProduces = operation.downField("produces", _.produces).map(_.flatMap(ContentType.unapply)).unwrapTracker
            val producesAnnotation = NonEmptyList
              .fromList(
                responses.value
                  .flatMap(getBestProduces[ScalaLanguage](operationId, allProduces, _, isTypePlain))
                  .distinct
                  .map(_.toJaxRsAnnotationName)
              )
              .map(producesTerms => mod"@Produces(Array(..${producesTerms.toList}))")

            val methodAnnotations = pathAnnotation.toList ++ List(httpMethodAnnotation) ++ consumesAnnotation ++ producesAnnotation

            val formParamAnnot = if (consumes.contains(MultipartFormData)) "FormDataParam" else "FormParam"
            val methodParams =
              parameters.pathParams.map(paramTransformers.transform(_, Some("PathParam"))) ++
                  parameters.headerParams.map(paramTransformers.transform(_, Some("HeaderParam"))) ++
                  parameters.queryStringParams.map(paramTransformers.transform(_, Some("QueryParam"))) ++
                  parameters.formParams.map(paramTransformers.transform(_, Some(formParamAnnot))) ++
                  parameters.bodyParams.filter(_ => parameters.formParams.isEmpty).map(paramTransformers.transform(_, None)) ++
                  List(param"@Suspended asyncResponse: AsyncResponse")

            val handlerParams = (
              parameters.pathParams.map(_.param) ++
                  parameters.headerParams.map(_.param) ++
                  parameters.queryStringParams.map(_.param) ++
                  parameters.formParams.map(param => paramTransformers.replaceFileParam(param.in, param.isFile)(param.param)) ++
                  parameters.bodyParams.filter(_ => parameters.formParams.isEmpty).map(_.param)
            ).map(paramTransformers.stripOptionFromCollections).map(_.copy(default = None))

            val handlerArgs = handlerParams.map({ param =>
              val nameTerm = Term.Name(param.name.value)
              param.decltpe.fold[Term](nameTerm)({
                case Type.Select(Term.Select(q"java", q"time"), _)                              => q"$nameTerm.get"
                case Type.Apply(_, List(Type.Select(Term.Select(q"java", q"time"), _)))         => q"$nameTerm.map(_.get)"
                case Type.Apply(t"Map", List(_, Type.Select(Term.Select(q"java", q"time"), _))) => q"$nameTerm.mapValues(_.get)"
                case _                                                                          => nameTerm
              })
            })

            val responseCases = responses.value.map({ response =>
              val responseClsSubName = s"$responseClsName${response.statusCodeName}"
              val responseClsSubTerm = Term.Name(responseClsSubName)
              response.value.fold(
                p"case $resourceNameTerm.$responseClsSubTerm => responseBuilder.build()"
              )({ _ =>
                p"case ${Pat.Extract(Term.Select(resourceNameTerm, responseClsSubTerm), List(p"value"))} => responseBuilder.entity(value).build()"
              })
            })

            val methodNameTerm  = Term.Name(methodName)
            val responseClsTerm = Term.Select(resourceNameTerm, Term.Name(responseClsName))
            val responseClsType = Type.Name(responseClsName)
            val respondParam    = param"respond: ${Type.Singleton(responseClsTerm)}"

            val routeMethod = q"""
             ..$methodAnnotations
             def ${Term.Name(methodName)}(..$methodParams): Unit =
               this.handler.$methodNameTerm($responseClsTerm)(..$handlerArgs).onComplete({
                 case scala.util.Success(result) =>
                   val responseBuilder = Response.status(result.statusCode)
                   val response = ${Term.Match(Term.Name("result"), responseCases)}
                   asyncResponse.resume(response)
                 case scala.util.Failure(err) =>
                   logger.error("{} threw an exception ({}): {}", ${Lit.String(s"$resourceName.$methodName")}, err.getClass.getName, err.getMessage, err)
                   asyncResponse.resume(Response.status(Status.INTERNAL_SERVER_ERROR).build())
               })
           """

            val handlerRetTypeParam = t"$resourceNameTerm.$responseClsType"
            val handlerMethodSig    = q"def $methodNameTerm($respondParam)(..$handlerParams): scala.concurrent.Future[$handlerRetTypeParam]"

            (routeMethod: Stat, handlerMethodSig)
        })
        .unzip

      val classPathAnnotation = mod"@Path(${Lit.String(fullPathPrefix)})"

      val supportDefinitions = List(
        q"private val logger = LoggerFactory.getLogger(getClass)"
      )

      Target.pure(
        RenderedRoutes[ScalaLanguage](
          routeMethods,
          List(classPathAnnotation),
          handlerMethodSigs,
          supportDefinitions,
          List.empty
        )
      )
    }

    override def getExtraRouteParams(customExtraction: Boolean, tracing: Boolean): Target[List[Term.Param]] =
      for {
        customExtraction <- if (customExtraction) {
          Target.raiseUserError(s"Custom Extraction is not yet supported by this framework")
        } else Target.pure(List.empty)

        tracing <- if (tracing) {
          Target.raiseUserError(s"Tracing is not yet supported by this framework")
        } else Target.pure(List.empty)
      } yield (customExtraction ::: tracing)

    override def renderClass(
        resourceName: String,
        handlerName: String,
        annotations: List[Mod.Annot],
        combinedRouteTerms: List[Stat],
        extraRouteParams: List[Term.Param],
        responseDefinitions: List[Defn],
        supportDefinitions: List[Defn],
        customExtraction: Boolean
    ): Target[List[Defn]] = {
      val routeParams = param"handler: ${Type.Name(handlerName)}" +: extraRouteParams
      Target.pure(
        List(
          q"""
             ..$annotations
             class ${Type.Name(resourceName)}(..$routeParams)(implicit ec: ExecutionContext) {
               ..$supportDefinitions
               ..$combinedRouteTerms
             }
           """,
          q"""
             object ${Term.Name(resourceName)} {
               ..$responseDefinitions
             }
           """
        )
      )
    }

    override def renderHandler(
        handlerName: String,
        methodSigs: List[Decl.Def],
        handlerDefinitions: List[Stat],
        responseDefinitions: List[Defn],
        customExtraction: Boolean
    ): Target[Defn] =
      Target.pure(
        q"""
          trait ${Type.Name(handlerName)} {
            ..${methodSigs ++ handlerDefinitions}
          }
         """
      )
  }
}
