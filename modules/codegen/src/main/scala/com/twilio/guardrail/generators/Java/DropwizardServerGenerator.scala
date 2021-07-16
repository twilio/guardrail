package com.twilio.guardrail.generators.Java

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.all._
import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.Modifier.Keyword._
import com.github.javaparser.ast.Modifier._
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, PrimitiveType, Type, UnknownType, VoidType }
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.expr.{ FieldAccessExpr, MethodCallExpr, _ }
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.{ ImportDeclaration, Node, NodeList }
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.extract.ServerRawResponse
import com.twilio.guardrail.generators.LanguageParameter
import com.twilio.guardrail.generators.helpers.DropwizardHelpers._
import com.twilio.guardrail.generators.syntax.Java._
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.protocol.terms._
import com.twilio.guardrail.protocol.terms.server._
import com.twilio.guardrail.shims.OperationExt
import com.twilio.guardrail.terms.collections.CollectionsAbstraction
import com.twilio.guardrail.terms.{ CollectionsLibTerms, RouteMeta, SecurityScheme }
import com.twilio.guardrail.{ CustomExtractionField, RenderedRoutes, StrictProtocolElems, SupportDefinition, Target, TracingField }
import io.swagger.v3.oas.models.Operation
import scala.compat.java8.OptionConverters._
import scala.concurrent.Future
import scala.language.existentials

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Null"))
object DropwizardServerGenerator {
  private implicit class ContentTypeExt(private val ct: ContentType) extends AnyVal {
    def toJaxRsAnnotationName: Expression = ct match {
      case ApplicationJson     => new FieldAccessExpr(new NameExpr("MediaType"), "APPLICATION_JSON")
      case UrlencodedFormData  => new FieldAccessExpr(new NameExpr("MediaType"), "APPLICATION_FORM_URLENCODED")
      case MultipartFormData   => new FieldAccessExpr(new NameExpr("MediaType"), "MULTIPART_FORM_DATA")
      case TextPlain           => new FieldAccessExpr(new NameExpr("MediaType"), "TEXT_PLAIN")
      case OctetStream         => new FieldAccessExpr(new NameExpr("MediaType"), "APPLICATION_OCTET_STREAM")
      case TextContent(name)   => new StringLiteralExpr(name)
      case BinaryContent(name) => new StringLiteralExpr(name)
    }
  }

  private val ASYNC_RESPONSE_TYPE   = StaticJavaParser.parseClassOrInterfaceType("AsyncResponse")
  private val RESPONSE_TYPE         = StaticJavaParser.parseClassOrInterfaceType("Response")
  private val RESPONSE_BUILDER_TYPE = StaticJavaParser.parseClassOrInterfaceType("Response.ResponseBuilder")
  private val LOGGER_TYPE           = StaticJavaParser.parseClassOrInterfaceType("Logger")
  private val FILE_TYPE             = StaticJavaParser.parseClassOrInterfaceType("java.io.File")
  private val REQUEST_TIMEOUT_TYPE  = StaticJavaParser.parseClassOrInterfaceType("RequestTimeout")

  private val INSTANT_PARAM_TYPE          = StaticJavaParser.parseClassOrInterfaceType("GuardrailJerseySupport.Jsr310.InstantParam")
  private val OFFSET_DATE_TIME_PARAM_TYPE = StaticJavaParser.parseClassOrInterfaceType("GuardrailJerseySupport.Jsr310.OffsetDateTimeParam")
  private val ZONED_DATE_TIME_PARAM_TYPE  = StaticJavaParser.parseClassOrInterfaceType("GuardrailJerseySupport.Jsr310.ZonedDateTimeParam")
  private val LOCAL_DATE_TIME_PARAM_TYPE  = StaticJavaParser.parseClassOrInterfaceType("GuardrailJerseySupport.Jsr310.LocalDateTimeParam")
  private val LOCAL_DATE_PARAM_TYPE       = StaticJavaParser.parseClassOrInterfaceType("GuardrailJerseySupport.Jsr310.LocalDateParam")
  private val LOCAL_TIME_PARAM_TYPE       = StaticJavaParser.parseClassOrInterfaceType("GuardrailJerseySupport.Jsr310.LocalTimeParam")
  private val OFFSET_TIME_PARAM_TYPE      = StaticJavaParser.parseClassOrInterfaceType("GuardrailJerseySupport.Jsr310.OffsetTimeParam")
  private val DURATION_PARAM_TYPE         = StaticJavaParser.parseClassOrInterfaceType("GuardrailJerseySupport.Jsr310.DurationParam")

  def generateResponseSuperClass(name: String): Target[ClassOrInterfaceDeclaration] =
    Target.log.function("generateResponseSuperClass") {
      for {
        _ <- Target.log.info(s"Name: ${name}")
        cls = new ClassOrInterfaceDeclaration(new NodeList(abstractModifier), false, name)

        _ = cls.addAnnotation(generatedAnnotation(getClass))

        _ = cls.addField(PrimitiveType.intType, "statusCode", PRIVATE, FINAL)

        _ = cls
          .addConstructor()
          .addParameter(new Parameter(new NodeList(finalModifier), PrimitiveType.intType, new SimpleName("statusCode")))
          .setBody(
            new BlockStmt(
              new NodeList(
                new ExpressionStmt(new AssignExpr(new FieldAccessExpr(new ThisExpr, "statusCode"), new NameExpr("statusCode"), AssignExpr.Operator.ASSIGN))
              )
            )
          )

        _ = cls
          .addMethod(s"getStatusCode", PUBLIC)
          .setType(PrimitiveType.intType)
          .setBody(
            new BlockStmt(
              new NodeList(
                new ReturnStmt(new FieldAccessExpr(new ThisExpr, "statusCode"))
              )
            )
          )
      } yield cls
    }

  def generateResponseClass(
      superClassType: ClassOrInterfaceType,
      response: Response[JavaLanguage],
      errorEntityFallbackType: Option[Type]
  ): Target[(ClassOrInterfaceDeclaration, BodyDeclaration[_ <: BodyDeclaration[_]])] = {
    val clsName = response.statusCodeName.asString
    for {
      clsType <- safeParseClassOrInterfaceType(clsName)
    } yield {
      val cls = new ClassOrInterfaceDeclaration(new NodeList(publicModifier, staticModifier), false, clsName)
        .setExtendedTypes(new NodeList(superClassType))
        .addAnnotation(generatedAnnotation(getClass))

      val (classDecls, creator) = response.value
        .map(_._2)
        .orElse({
          if (response.statusCode >= 400 && response.statusCode <= 599) {
            errorEntityFallbackType
          } else {
            None
          }
        })
        .fold[(List[BodyDeclaration[_ <: BodyDeclaration[_]]], BodyDeclaration[_ <: BodyDeclaration[_]])]({
          val constructor = new ConstructorDeclaration(new NodeList(privateModifier), clsName)
          val _ = constructor.setBody(
            new BlockStmt(
              new NodeList(
                new ExpressionStmt(
                  new MethodCallExpr(
                    "super",
                    new IntegerLiteralExpr(response.statusCode.toString)
                  )
                )
              )
            )
          )

          val creator = new FieldDeclaration(
            new NodeList(publicModifier, staticModifier, finalModifier),
            new VariableDeclarator(clsType, clsName, new ObjectCreationExpr(null, clsType, new NodeList))
          )

          (List(constructor), creator)
        })({ valueType =>
          val constructParam = new Parameter(new NodeList(finalModifier), valueType.unbox, new SimpleName("entityBody"))

          val constructor = new ConstructorDeclaration(new NodeList(privateModifier), clsName)
            .addParameter(constructParam)
            .setBody(
              new BlockStmt(
                new NodeList(
                  new ExpressionStmt(
                    new MethodCallExpr(
                      "super",
                      new IntegerLiteralExpr(response.statusCode.toString)
                    )
                  ),
                  new ExpressionStmt(
                    new AssignExpr(
                      new FieldAccessExpr(new ThisExpr, constructParam.getNameAsString),
                      constructParam.getNameAsExpression,
                      AssignExpr.Operator.ASSIGN
                    )
                  )
                )
              )
            )

          val entityBodyField = new FieldDeclaration(
            new NodeList(privateModifier, finalModifier),
            new VariableDeclarator(valueType, "entityBody")
          )

          val entityBodyGetter = new MethodDeclaration(new NodeList(publicModifier), valueType, "getEntityBody")
            .setBody(
              new BlockStmt(
                new NodeList(
                  new ReturnStmt(new FieldAccessExpr(new ThisExpr, "entityBody"))
                )
              )
            )

          val creator = new MethodDeclaration(new NodeList(publicModifier, staticModifier), clsType, clsName)
            .addParameter(constructParam)
            .setBody(
              new BlockStmt(
                new NodeList(
                  new ReturnStmt(new ObjectCreationExpr(null, clsType, new NodeList(constructParam.getNameAsExpression)))
                )
              )
            )

          (List(constructor, entityBodyField, entityBodyGetter), creator)
        })

      sortDefinitions(classDecls).foreach(cls.addMember)
      (cls, creator)
    }
  }

  def ServerTermInterp(
      implicit Cl: CollectionsLibTerms[JavaLanguage, Target],
      Ca: CollectionsAbstraction[JavaLanguage]
  ): ServerTerms[JavaLanguage, Target] = new ServerTermInterp

  class ServerTermInterp(implicit Cl: CollectionsLibTerms[JavaLanguage, Target], Ca: CollectionsAbstraction[JavaLanguage])
      extends ServerTerms[JavaLanguage, Target] {
    import Ca._

    implicit def MonadF: Monad[Target] = Target.targetInstances

    override def getExtraImports(tracing: Boolean, supportPackage: NonEmptyList[String]): Target[List[ImportDeclaration]] =
      List(
        "javax.inject.Inject",
        "javax.validation.constraints.NotNull",
        "javax.ws.rs.Consumes",
        "javax.ws.rs.DELETE",
        "javax.ws.rs.FormParam",
        "javax.ws.rs.GET",
        "javax.ws.rs.HEAD",
        "javax.ws.rs.HeaderParam",
        "javax.ws.rs.OPTIONS",
        "javax.ws.rs.POST",
        "javax.ws.rs.PUT",
        "javax.ws.rs.Path",
        "javax.ws.rs.PathParam",
        "javax.ws.rs.Produces",
        "javax.ws.rs.QueryParam",
        "javax.ws.rs.container.AsyncResponse",
        "javax.ws.rs.container.Suspended",
        "javax.ws.rs.core.MediaType",
        "javax.ws.rs.core.Response",
        "org.glassfish.jersey.media.multipart.FormDataParam",
        "org.hibernate.validator.valuehandling.UnwrapValidatedValue",
        "org.slf4j.Logger",
        "org.slf4j.LoggerFactory"
      ).traverse(safeParseRawImport)

    override def buildCustomExtractionFields(
        operation: Tracker[Operation],
        resourceName: List[String],
        customExtraction: Boolean
    ): Target[Option[CustomExtractionField[JavaLanguage]]] =
      if (customExtraction) {
        Target.raiseUserError(s"Custom Extraction is not yet supported by this framework")
      } else {
        Target.pure(Option.empty)
      }

    override def buildTracingFields(operation: Tracker[Operation], resourceName: List[String], tracing: Boolean): Target[Option[TracingField[JavaLanguage]]] =
      if (tracing) {
        Target.raiseUserError(s"Tracing is not yet supported by this framework")
      } else {
        Target.pure(Option.empty)
      }

    override def generateRoutes(
        tracing: Boolean,
        resourceName: String,
        handlerName: String,
        basePath: Option[String],
        routes: List[GenerateRouteMeta[JavaLanguage]],
        protocolElems: List[StrictProtocolElems[JavaLanguage]],
        securitySchemes: Map[String, SecurityScheme[JavaLanguage]]
    ): Target[RenderedRoutes[JavaLanguage]] =
      for {
        resourceType <- safeParseClassOrInterfaceType(resourceName)
        handlerType  <- safeParseClassOrInterfaceType(handlerName)
        basePathComponents = basePath.toList.flatMap(splitPathComponents)
        commonPathPrefix   = findPathPrefix(routes.map(_.routeMeta.path.unwrapTracker))
        routeMethodsAndHandlerMethodSigs <- routes
          .traverse({
            case GenerateRouteMeta(
                operationId,
                methodName,
                responseClsName,
                customExtractionFields,
                tracingFields,
                sr @ RouteMeta(path, httpMethod, operation, securityRequirements),
                parameters,
                responses
                ) =>
              parameters.parameters.foreach(p => p.param.setType(p.param.getType.unbox))

              val method = new MethodDeclaration(new NodeList(publicModifier), new VoidType, methodName)
                .addAnnotation(new MarkerAnnotationExpr(httpMethod.toString))

              val pathSuffix = splitPathComponents(path.unwrapTracker).drop(commonPathPrefix.length).mkString("/", "/", "")
              if (pathSuffix.nonEmpty && pathSuffix != "/") {
                method.addAnnotation(new SingleMemberAnnotationExpr(new Name("Path"), new StringLiteralExpr(pathSuffix)))
              }

              val allConsumes = operation.downField("consumes", _.consumes).map(_.flatMap(ContentType.unapply)).unwrapTracker
              val consumes    = getBestConsumes(operation, allConsumes, parameters)
              consumes
                .map(c => new SingleMemberAnnotationExpr(new Name("Consumes"), c.toJaxRsAnnotationName))
                .foreach(method.addAnnotation)

              val allProduces = operation.downField("produces", _.produces).map(_.flatMap(ContentType.unapply)).unwrapTracker
              NonEmptyList
                .fromList(
                  responses.value
                    .flatMap(getBestProduces[JavaLanguage](operationId, allProduces, _, _.isPlain))
                    .distinct
                    .map(_.toJaxRsAnnotationName)
                )
                .foreach(
                  producesExprs =>
                    method.addAnnotation(
                      new SingleMemberAnnotationExpr(
                        new Name("Produces"),
                        producesExprs.toList match {
                          case singleProduces :: Nil => singleProduces
                          case manyProduces          => new ArrayInitializerExpr(manyProduces.toNodeList)
                        }
                      )
                    )
                )

              def transformJsr310Params(parameter: Parameter): Target[Parameter] = {
                val isOptional = parameter.getType.isOptionalType
                val tpe        = if (isOptional) parameter.getType.containedType else parameter.getType

                def transform(to: Type): Target[Parameter] = {
                  parameter.setType(if (isOptional) to.liftOptionalType else to)
                  if (!isOptional) {
                    parameter.getAnnotations.add(0, new MarkerAnnotationExpr("UnwrapValidatedValue"))
                  }
                  Target.pure(parameter)
                }

                tpe match {
                  case cls: ClassOrInterfaceType if cls.getScope.asScala.forall(_.asString == "java.time") =>
                    cls.getNameAsString match {
                      case "Instant"        => transform(INSTANT_PARAM_TYPE)
                      case "OffsetDateTime" => transform(OFFSET_DATE_TIME_PARAM_TYPE)
                      case "ZonedDateTime"  => transform(ZONED_DATE_TIME_PARAM_TYPE)
                      case "LocalDateTime"  => transform(LOCAL_DATE_TIME_PARAM_TYPE)
                      case "LocalDate"      => transform(LOCAL_DATE_PARAM_TYPE)
                      case "LocalTime"      => transform(LOCAL_TIME_PARAM_TYPE)
                      case "OffsetTime"     => transform(OFFSET_TIME_PARAM_TYPE)
                      case "Duration"       => transform(DURATION_PARAM_TYPE)
                      case _                => Target.pure(parameter)
                    }
                  case _ => Target.pure(parameter)
                }
              }

              // When we have a file inside multipart/form-data, we don't want to use InputStream,
              // because that will require the server to buffer the entire contents in memory as it
              // reads in the entire body.  Instead we instruct Dropwizard to write it out to a file
              // on disk and use java.io.File.
              def transformMultipartFile(parameter: Parameter, param: LanguageParameter[JavaLanguage]): Target[Parameter] =
                (param.isFile, param.required) match {
                  case (true, true)  => Target.pure(parameter.setType(FILE_TYPE))
                  case (true, false) => Cl.liftOptionalType(FILE_TYPE).map(parameter.setType)
                  case _             => Target.pure(parameter)
                }

              def addValidationAnnotations(parameter: Parameter, param: LanguageParameter[JavaLanguage]): Parameter = {
                if (param.required) {
                  // NB: The order here is actually critical.  In the case where we're using multipart,
                  // the @NotNull annotation *must* come before the @FormDataParam annotation.  See:
                  // https://github.com/eclipse-ee4j/jersey/issues/3632
                  parameter.getAnnotations.add(0, new MarkerAnnotationExpr("NotNull"))

                  // Vavr's validation support for some reason requires this.
                  if (param.param.getTypeAsString.startsWith("io.vavr.collection.")) {
                    parameter.getAnnotations.add(1, new MarkerAnnotationExpr("UnwrapValidatedValue"))
                  }
                }
                parameter
              }

              def stripOptionalFromCollections(parameter: Parameter, param: LanguageParameter[JavaLanguage]): Parameter =
                if (!param.required && parameter.getType.containedType.isListType) {
                  parameter.setType(parameter.getType.containedType)
                } else {
                  parameter
                }

              def addParamAnnotation(parameter: Parameter, param: LanguageParameter[JavaLanguage], annotationName: String): Parameter =
                parameter.addAnnotation(new SingleMemberAnnotationExpr(new Name(annotationName), new StringLiteralExpr(param.argName.value)))

              def boxParameterTypes(parameter: Parameter): Parameter = {
                if (parameter.getType.isPrimitiveType) {
                  parameter.setType(parameter.getType.asPrimitiveType.toBoxedType)
                }
                parameter
              }

              def transformHandlerArg(parameter: Parameter): Expression = {
                val isOptional = parameter.getType.isOptionalType
                val typeName   = if (isOptional) parameter.getType.containedType.asString else parameter.getType.asString
                if (typeName.startsWith("GuardrailJerseySupport.Jsr310.") && typeName.endsWith("Param")) {
                  if (isOptional) {
                    new MethodCallExpr(
                      parameter.getNameAsExpression,
                      "map",
                      new NodeList[Expression](new MethodReferenceExpr(new NameExpr(typeName), new NodeList, "get"))
                    )
                  } else {
                    new MethodCallExpr(parameter.getNameAsExpression, "get")
                  }
                } else {
                  parameter.getNameAsExpression
                }
              }

              for {
                annotatedMethodParams <- List(
                  (parameters.pathParams, "PathParam"),
                  (parameters.headerParams, "HeaderParam"),
                  (parameters.queryStringParams, "QueryParam"),
                  (parameters.formParams, if (consumes.contains(MultipartFormData)) "FormDataParam" else "FormParam")
                ).flatTraverse({
                  case (params, annotationName) =>
                    params.traverse({ param =>
                      val parameter                  = param.param.clone()
                      val optionalCollectionStripped = stripOptionalFromCollections(parameter, param)
                      val annotated                  = addParamAnnotation(optionalCollectionStripped, param, annotationName)
                      for {
                        dateTransformed <- transformJsr310Params(annotated)
                        fileTransformed <- transformMultipartFile(dateTransformed, param)
                      } yield addValidationAnnotations(fileTransformed, param)
                    })
                })

                bareMethodParams <- parameters.bodyParams.toList
                  .traverse({ param =>
                    val parameter                  = param.param.clone()
                    val optionalCollectionStripped = stripOptionalFromCollections(parameter, param)
                    for {
                      dateTransformed <- transformJsr310Params(optionalCollectionStripped)
                    } yield addValidationAnnotations(dateTransformed, param)
                  })

                methodParams = (annotatedMethodParams ++ bareMethodParams).map(boxParameterTypes)
                _            = methodParams.foreach(method.addParameter)
                _ = method.addParameter(
                  new Parameter(new NodeList(finalModifier), ASYNC_RESPONSE_TYPE, new SimpleName("asyncResponse")).addMarkerAnnotation("Suspended")
                )

                (responseType, resultResumeBody) = ServerRawResponse(operation)
                  .filter(_ == true)
                  .fold({
                    val responseName = s"$handlerName.$responseClsName"
                    val entitySetterIfTree = NonEmptyList
                      .fromList(responses.value.collect({
                        case Response(statusCodeName, Some(_), _) => statusCodeName
                      }))
                      .map(_.reverse.foldLeft[IfStmt](null)({
                        case (nextIfTree, statusCodeName) =>
                          val responseSubclassType = StaticJavaParser.parseClassOrInterfaceType(s"${responseName}.${statusCodeName}")
                          new IfStmt(
                            new InstanceOfExpr(new NameExpr("result"), responseSubclassType),
                            new BlockStmt(
                              new NodeList(
                                new ExpressionStmt(
                                  new MethodCallExpr(
                                    new NameExpr("builder"),
                                    "entity",
                                    new NodeList[Expression](
                                      new MethodCallExpr(
                                        new EnclosedExpr(new CastExpr(responseSubclassType, new NameExpr("result"))),
                                        "getEntityBody"
                                      )
                                    )
                                  )
                                )
                              )
                            ),
                            nextIfTree
                          )
                      }))
                    (
                      StaticJavaParser.parseClassOrInterfaceType(responseName),
                      (
                        List[Statement](
                          new ExpressionStmt(
                            new VariableDeclarationExpr(
                              new VariableDeclarator(
                                RESPONSE_BUILDER_TYPE,
                                "builder",
                                new MethodCallExpr(
                                  new NameExpr("Response"),
                                  "status",
                                  new NodeList[Expression](new MethodCallExpr(new NameExpr("result"), "getStatusCode"))
                                )
                              ),
                              finalModifier
                            )
                          )
                        ) ++ entitySetterIfTree ++ List(
                              new ExpressionStmt(
                                new MethodCallExpr(
                                  new NameExpr("asyncResponse"),
                                  "resume",
                                  new NodeList[Expression](new MethodCallExpr(new NameExpr("builder"), "build"))
                                )
                              )
                            )
                      ).toNodeList
                    )
                  })({ _ =>
                    (
                      RESPONSE_TYPE,
                      new NodeList(
                        new ExpressionStmt(
                          new MethodCallExpr(
                            new NameExpr("asyncResponse"),
                            "resume",
                            new NodeList[Expression](new NameExpr("result"))
                          )
                        )
                      )
                    )
                  })

                resultErrorBody = List[Statement](
                  new ExpressionStmt(
                    new MethodCallExpr(
                      new NameExpr("logger"),
                      "error",
                      new NodeList[Expression](
                        new StringLiteralExpr(s"${handlerName}.${methodName} threw an exception ({}): {}"),
                        new MethodCallExpr(new MethodCallExpr(new NameExpr("err"), "getClass"), "getName"),
                        new MethodCallExpr(new NameExpr("err"), "getMessage"),
                        new NameExpr("err")
                      )
                    )
                  ),
                  new ExpressionStmt(
                    new MethodCallExpr(
                      new NameExpr("asyncResponse"),
                      "resume",
                      new NodeList[Expression](
                        new MethodCallExpr(
                          new MethodCallExpr(
                            new NameExpr("Response"),
                            "status",
                            new NodeList[Expression](new IntegerLiteralExpr("500"))
                          ),
                          "build"
                        )
                      )
                    )
                  )
                )

                timeoutSetter = new MethodCallExpr(s"get${methodName.capitalize}RequestTimeout")
                  .lift[Option[Any]]
                  .foreach(
                    new LambdaExpr(
                      new Parameter(new UnknownType, "_requestTimeout"),
                      new BlockStmt(
                        new NodeList(
                          new ExpressionStmt(
                            new MethodCallExpr(
                              new NameExpr("asyncResponse"),
                              "setTimeout",
                              new NodeList[Expression](
                                new MethodCallExpr(new MethodCallExpr(new NameExpr("_requestTimeout"), "getTimeout"), "toNanos"),
                                new FieldAccessExpr(new NameExpr("java.util.concurrent.TimeUnit"), "NANOSECONDS")
                              )
                            )
                          ),
                          new ExpressionStmt(
                            new MethodCallExpr(
                              new NameExpr("asyncResponse"),
                              "setTimeoutHandler",
                              new NodeList[Expression](
                                new LambdaExpr(
                                  new Parameter(new UnknownType, "t"),
                                  new BlockStmt(
                                    new NodeList(
                                      new IfStmt(
                                        new UnaryExpr(new MethodCallExpr(new NameExpr("asyncResponse"), "isDone"), UnaryExpr.Operator.LOGICAL_COMPLEMENT),
                                        new BlockStmt(
                                          new NodeList(
                                            new ExpressionStmt(
                                              new MethodCallExpr(
                                                new NameExpr("asyncResponse"),
                                                "resume",
                                                new NodeList[Expression](new MethodCallExpr(new NameExpr("_requestTimeout"), "getTimeoutResponse"))
                                              )
                                            )
                                          )
                                        ),
                                        null
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    ).lift[Any => Unit]
                  )
                  .value

                handlerCall = new MethodCallExpr(
                  new FieldAccessExpr(new ThisExpr, "handler"),
                  methodName,
                  new NodeList[Expression](methodParams.map(transformHandlerArg): _*)
                )

                _ = method.setBody(
                  new BlockStmt(
                    new NodeList(
                      new ExpressionStmt(timeoutSetter),
                      new ExpressionStmt(
                        handlerCall
                          .lift[Future[Any]]
                          .onComplete[Throwable, Expression](
                            new LambdaExpr(
                              new Parameter(new UnknownType, "result"),
                              new BlockStmt(resultResumeBody)
                            ).lift[Any => Unit],
                            new LambdaExpr(
                              new Parameter(new UnknownType, "err"),
                              new BlockStmt(resultErrorBody.toNodeList)
                            ).lift[Throwable => Unit]
                          )
                          .value
                      )
                    )
                  )
                )

                requestTimeoutGetter = new MethodDeclaration(
                  new NodeList(protectedModifier),
                  REQUEST_TIMEOUT_TYPE.liftOptionalType,
                  s"get${methodName.capitalize}RequestTimeout"
                ).setBody(
                  new BlockStmt(
                    new NodeList(
                      new ReturnStmt(new FieldAccessExpr(new ThisExpr, "requestTimeout"))
                    )
                  )
                )

                transformedAnnotatedParams <- (
                  parameters.pathParams ++
                      parameters.headerParams ++
                      parameters.queryStringParams ++
                      parameters.formParams
                ).traverse({ param =>
                  val parameter                  = param.param.clone()
                  val optionalCollectionStripped = stripOptionalFromCollections(parameter, param)
                  transformMultipartFile(optionalCollectionStripped, param)
                })
                transformedBodyParams = parameters.bodyParams.map(param => stripOptionalFromCollections(param.param.clone(), param))
              } yield {
                val futureResponseType = responseType.liftFutureType
                val handlerMethodSig   = new MethodDeclaration(new NodeList(), futureResponseType, methodName)
                (transformedAnnotatedParams ++ transformedBodyParams).foreach(handlerMethodSig.addParameter)
                handlerMethodSig.setBody(null)

                (List(method, requestTimeoutGetter), handlerMethodSig)
              }
          })
          .map(_.unzip)
        (routeMethods, handlerMethodSigs) = routeMethodsAndHandlerMethodSigs
      } yield {
        import Ca._

        val resourceConstructorNoTimeout = new ConstructorDeclaration(new NodeList(publicModifier), resourceName)
          .addAnnotation(new MarkerAnnotationExpr(new Name("Inject")))
          .addParameter(new Parameter(new NodeList(finalModifier), handlerType, new SimpleName("handler")))
          .setBody(
            new BlockStmt(
              new NodeList(
                new ExpressionStmt(new AssignExpr(new FieldAccessExpr(new ThisExpr, "handler"), new NameExpr("handler"), AssignExpr.Operator.ASSIGN)),
                new ExpressionStmt(new AssignExpr(new FieldAccessExpr(new ThisExpr, "requestTimeout"), emptyOptional.value, AssignExpr.Operator.ASSIGN))
              )
            )
          )

        val resourceConstructorWithTimeout = new ConstructorDeclaration(new NodeList(publicModifier), resourceName)
          .setParameters(
            new NodeList(
              new Parameter(new NodeList(finalModifier), handlerType, new SimpleName("handler")),
              new Parameter(new NodeList(finalModifier), REQUEST_TIMEOUT_TYPE, new SimpleName("requestTimeout"))
            )
          )
          .setBody(
            new BlockStmt(
              new NodeList(
                new ExpressionStmt(new AssignExpr(new FieldAccessExpr(new ThisExpr, "handler"), new NameExpr("handler"), AssignExpr.Operator.ASSIGN)),
                new ExpressionStmt(
                  new AssignExpr(
                    new FieldAccessExpr(new ThisExpr, "requestTimeout"),
                    new NameExpr("requestTimeout").lift[Any].liftOptional.value,
                    AssignExpr.Operator.ASSIGN
                  )
                )
              )
            )
          )

        val annotations = List(
          new SingleMemberAnnotationExpr(new Name("Path"), new StringLiteralExpr((basePathComponents ++ commonPathPrefix).mkString("/", "/", "")))
        )

        val supportDefinitions = List[BodyDeclaration[_ <: BodyDeclaration[_]]](
          new FieldDeclaration(
            new NodeList(privateModifier, staticModifier, finalModifier),
            new VariableDeclarator(
              LOGGER_TYPE,
              "logger",
              new MethodCallExpr(new NameExpr("LoggerFactory"), "getLogger", new NodeList[Expression](new ClassExpr(resourceType)))
            )
          ),
          new FieldDeclaration(new NodeList(privateModifier, finalModifier), new VariableDeclarator(handlerType, "handler")),
          new FieldDeclaration(new NodeList(privateModifier, finalModifier), new VariableDeclarator(REQUEST_TIMEOUT_TYPE.liftOptionalType, "requestTimeout")),
          resourceConstructorWithTimeout,
          resourceConstructorNoTimeout
        )

        RenderedRoutes[JavaLanguage](routeMethods.flatten, annotations, handlerMethodSigs, supportDefinitions, List.empty)
      }

    override def getExtraRouteParams(customExtraction: Boolean, tracing: Boolean): Target[List[Parameter]] =
      for {
        customExtraction <- if (customExtraction) {
          Target.raiseUserError(s"Custom Extraction is not yet supported by this framework")
        } else Target.pure(List.empty)

        tracing <- if (tracing) {
          Target.raiseUserError(s"Tracing is not yet supported by this framework")
        } else Target.pure(List.empty)
      } yield (customExtraction ::: tracing)

    override def generateResponseDefinitions(
        responseClsName: String,
        responses: Responses[JavaLanguage],
        protocolElems: List[StrictProtocolElems[JavaLanguage]]
    ): Target[List[BodyDeclaration[_ <: BodyDeclaration[_]]]] =
      for {
        abstractResponseClassType <- safeParseClassOrInterfaceType(responseClsName)

        // TODO: verify valueTypes are in protocolElems

        abstractResponseClass <- generateResponseSuperClass(responseClsName)
        responseClasses       <- responses.value.traverse(resp => generateResponseClass(abstractResponseClassType, resp, None))
      } yield {
        sortDefinitions(responseClasses.flatMap({ case (cls, creator) => List[BodyDeclaration[_ <: BodyDeclaration[_]]](cls, creator) }))
          .foreach(abstractResponseClass.addMember)

        abstractResponseClass :: Nil
      }

    override def generateSupportDefinitions(
        tracing: Boolean,
        securitySchemes: Map[String, SecurityScheme[JavaLanguage]]
    ): Target[List[SupportDefinition[JavaLanguage]]] =
      for {
        annotationImports <- List(
          "java.lang.annotation.ElementType",
          "java.lang.annotation.Retention",
          "java.lang.annotation.RetentionPolicy",
          "java.lang.annotation.Target",
          "javax.ws.rs.HttpMethod"
        ).traverse(safeParseRawImport)

        jersey         <- SerializationHelpers.guardrailJerseySupportDef
        requestTimeout <- requestTimeoutSupportDef
      } yield {
        def httpMethodAnnotation(name: String): SupportDefinition[JavaLanguage] = {
          val annotationDecl = new AnnotationDeclaration(new NodeList(publicModifier), name)
            .addAnnotation(
              new SingleMemberAnnotationExpr(
                new Name("Target"),
                new ArrayInitializerExpr(new NodeList(new FieldAccessExpr(new NameExpr("ElementType"), "METHOD")))
              )
            )
            .addAnnotation(new SingleMemberAnnotationExpr(new Name("Retention"), new FieldAccessExpr(new NameExpr("RetentionPolicy"), "RUNTIME")))
            .addAnnotation(new SingleMemberAnnotationExpr(new Name("HttpMethod"), new StringLiteralExpr(name)))
          SupportDefinition[JavaLanguage](new Name(name), annotationImports, List(annotationDecl))
        }
        List(
          jersey,
          requestTimeout,
          httpMethodAnnotation("PATCH"),
          httpMethodAnnotation("TRACE")
        )
      }

    override def renderClass(
        className: String,
        handlerName: String,
        classAnnotations: List[com.github.javaparser.ast.expr.AnnotationExpr],
        combinedRouteTerms: List[com.github.javaparser.ast.Node],
        extraRouteParams: List[com.github.javaparser.ast.body.Parameter],
        responseDefinitions: List[com.github.javaparser.ast.body.BodyDeclaration[_ <: com.github.javaparser.ast.body.BodyDeclaration[_]]],
        supportDefinitions: List[com.github.javaparser.ast.body.BodyDeclaration[_ <: com.github.javaparser.ast.body.BodyDeclaration[_]]],
        customExtraction: Boolean
    ): Target[List[BodyDeclaration[_ <: BodyDeclaration[_]]]] =
      safeParseSimpleName(className) >>
          safeParseSimpleName(handlerName) >>
          Target.pure(doRenderClass(className, classAnnotations, supportDefinitions, combinedRouteTerms) :: Nil)

    override def renderHandler(
        handlerName: String,
        methodSigs: List[com.github.javaparser.ast.body.MethodDeclaration],
        handlerDefinitions: List[com.github.javaparser.ast.Node],
        responseDefinitions: List[com.github.javaparser.ast.body.BodyDeclaration[_ <: com.github.javaparser.ast.body.BodyDeclaration[_]]],
        customExtraction: Boolean
    ): Target[BodyDeclaration[_ <: BodyDeclaration[_]]] = {
      val handlerClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier), true, handlerName)
      sortDefinitions(methodSigs ++ responseDefinitions).foreach(handlerClass.addMember)
      Target.pure(handlerClass)
    }

    // Lift this function out of RenderClass above to work around a 2.11.x compiler syntax bug
    private def doRenderClass(
        className: String,
        classAnnotations: List[AnnotationExpr],
        supportDefinitions: List[BodyDeclaration[_ <: BodyDeclaration[_]]],
        combinedRouteTerms: List[Node]
    ): ClassOrInterfaceDeclaration = {
      val cls = new ClassOrInterfaceDeclaration(new NodeList(publicModifier), false, className)
      classAnnotations.foreach(cls.addAnnotation)
      sortDefinitions(supportDefinitions ++ combinedRouteTerms.collect({ case bd: BodyDeclaration[_] => bd }))
        .foreach(cls.addMember)
      cls
    }
  }
}
