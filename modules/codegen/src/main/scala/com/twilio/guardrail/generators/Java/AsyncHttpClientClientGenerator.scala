package com.twilio.guardrail.generators.Java

import cats.data.NonEmptyList
import cats.implicits._
import cats.~>
import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.{ ImportDeclaration, NodeList }
import com.github.javaparser.ast.Modifier._
import com.github.javaparser.ast.Modifier.Keyword._
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, PrimitiveType, Type, UnknownType, VoidType }
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.expr.{ MethodCallExpr, NameExpr, _ }
import com.github.javaparser.ast.stmt._
import com.twilio.guardrail.generators.Java.AsyncHttpClientHelpers._
import com.twilio.guardrail.generators.ScalaParameter
import com.twilio.guardrail.generators.syntax.Java._
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.protocol.terms.{ ApplicationJson, BinaryContent, ContentType, MultipartFormData, Response, TextContent, UrlencodedFormData }
import com.twilio.guardrail.protocol.terms.client._
import com.twilio.guardrail.shims._
import com.twilio.guardrail.terms.RouteMeta
import com.twilio.guardrail.{ RenderedClientOperation, StaticDefns, SupportDefinition, SwaggerUtil, Target }
import java.net.URI

object AsyncHttpClientClientGenerator {
  private val URI_TYPE                       = StaticJavaParser.parseClassOrInterfaceType("URI")
  private val OBJECT_MAPPER_TYPE             = StaticJavaParser.parseClassOrInterfaceType("ObjectMapper")
  private val BUILDER_TYPE                   = StaticJavaParser.parseClassOrInterfaceType("Builder")
  private val MARSHALLING_EXCEPTION_TYPE     = StaticJavaParser.parseClassOrInterfaceType("MarshallingException")
  private val HTTP_ERROR_TYPE                = StaticJavaParser.parseClassOrInterfaceType("HttpError")
  private val EXCEPTION_TYPE                 = StaticJavaParser.parseClassOrInterfaceType("Exception")
  private val JSON_PROCESSING_EXCEPTION_TYPE = StaticJavaParser.parseClassOrInterfaceType("JsonProcessingException")
  private val CLIENT_EXCEPTION_TYPE          = StaticJavaParser.parseClassOrInterfaceType("ClientException")

  private val HTTP_CLIENT_FUNCTION_TYPE = functionType(REQUEST_TYPE, completionStageType(RESPONSE_TYPE))

  private def typeReferenceType(typeArg: Type): ClassOrInterfaceType =
    StaticJavaParser.parseClassOrInterfaceType("TypeReference").setTypeArguments(typeArg)

  private def showParam(param: ScalaParameter[JavaLanguage], overrideParamName: Option[String] = None): Expression = {
    val paramName = overrideParamName.getOrElse(param.paramName.asString)
    new MethodCallExpr(
      new MethodCallExpr(new NameExpr("Shower"), "getInstance"),
      "show",
      new NodeList[Expression](new NameExpr(paramName))
    )

    def doShow(tpe: Type): Expression = tpe match {
      case cls: ClassOrInterfaceType if cls.isOptional || cls.isNamed("List") =>
        doShow(cls.containedType)
      case _ =>
        new MethodCallExpr(
          new MethodCallExpr(new NameExpr("Shower"), "getInstance"),
          "show",
          new NodeList[Expression](new NameExpr(paramName))
        )
    }

    doShow(param.argType)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  private def generateBuilderMethodCall(param: ScalaParameter[JavaLanguage], builderMethodName: String, needsMultipart: Boolean): Statement = {
    val finalMethodName = if (needsMultipart) "addBodyPart" else builderMethodName
    val argName         = param.paramName.asString
    val isList          = param.argType.isNamed("List")

    val makeArgList: String => NodeList[Expression] = name =>
      if (param.isFile) {
        new NodeList[Expression](
          new ObjectCreationExpr(
            null,
            INPUT_STREAM_PART_TYPE,
            new NodeList(
              new StringLiteralExpr(param.argName.value),
              new NameExpr(name),
              new StringLiteralExpr(param.argName.value)
            )
          )
        )
      } else if (needsMultipart) {
        new NodeList[Expression](
          new ObjectCreationExpr(
            null,
            STRING_PART_TYPE,
            new NodeList(
              new StringLiteralExpr(param.argName.value),
              showParam(param, Some(name))
            )
          )
        )
      } else {
        new NodeList[Expression](new StringLiteralExpr(param.argName.value), showParam(param, Some(name)))
      }

    if (isList) {
      new ForEachStmt(
        new VariableDeclarationExpr(param.argType.containedType, "member", finalModifier),
        new NameExpr(argName),
        new BlockStmt(
          new NodeList(
            new ExpressionStmt(new MethodCallExpr(new NameExpr("builder"), finalMethodName, makeArgList("member")))
          )
        )
      )
    } else {
      new ExpressionStmt(new MethodCallExpr(new NameExpr("builder"), finalMethodName, makeArgList(argName)))
    }
  }

  private def generateBodyMethodCall(param: ScalaParameter[JavaLanguage], contentType: Option[ContentType]): Option[Statement] = {
    def wrapSetBody(expr: Expression): MethodCallExpr =
      new MethodCallExpr(new NameExpr("builder"), "setBody", new NodeList[Expression](expr))

    if (param.isFile) {
      Option(new ExpressionStmt(wrapSetBody(new NameExpr(param.paramName.asString))))
    } else {
      contentType match {
        case Some(ApplicationJson) =>
          Option(
            new TryStmt(
              new BlockStmt(
                new NodeList(
                  new ExpressionStmt(
                    wrapSetBody(
                      new MethodCallExpr(
                        new FieldAccessExpr(new ThisExpr, "objectMapper"),
                        "writeValueAsString",
                        new NodeList[Expression](new NameExpr(param.paramName.asString))
                      )
                    )
                  )
                )
              ),
              new NodeList(
                new CatchClause(
                  new Parameter(new NodeList(finalModifier), JSON_PROCESSING_EXCEPTION_TYPE, new SimpleName("e")),
                  new BlockStmt(
                    new NodeList(
                      new ThrowStmt(
                        new ObjectCreationExpr(
                          null,
                          MARSHALLING_EXCEPTION_TYPE,
                          new NodeList(new MethodCallExpr(new NameExpr("e"), "getMessage"), new NameExpr("e"))
                        )
                      )
                    )
                  )
                )
              ),
              null
            )
          )

        case Some(UrlencodedFormData) =>
          Option(
            new ExpressionStmt(
              new MethodCallExpr(
                new NameExpr("builder"),
                "setFormParams",
                new NodeList[Expression](
                  new MethodCallExpr(
                    new NameExpr("GuardrailJacksonFormUtils"),
                    "writeUrlEncodedForm",
                    new NodeList[Expression](
                      new FieldAccessExpr(new ThisExpr, "objectMapper"),
                      new NameExpr(param.paramName.asString)
                    )
                  )
                )
              )
            )
          )

        case Some(TextContent(_)) =>
          Option(
            new ExpressionStmt(
              wrapSetBody(
                new MethodCallExpr(
                  new MethodCallExpr(new NameExpr("Shower"), "getInstance"),
                  "show",
                  new NodeList[Expression](new NameExpr(param.paramName.asString))
                )
              )
            )
          )

        case Some(BinaryContent(_)) | None =>
          // FIXME: we're hoping that the type is something that AHC already supports
          Option(new ExpressionStmt(wrapSetBody(new NameExpr(param.paramName.asString))))

        case Some(MultipartFormData) =>
          // TODO: add support for POJO-to-multipart
          None
      }
    }
  }

  private def jacksonTypeReferenceFor(tpe: Type): Expression =
    tpe match {
      case cls: ClassOrInterfaceType if cls.getTypeArguments.isPresent =>
        new ObjectCreationExpr(null, typeReferenceType(cls), new NodeList).setAnonymousClassBody(new NodeList)
      case other =>
        new ClassExpr(other)
    }

  private def generateClientExceptionClasses(): Target[List[(List[ImportDeclaration], ClassOrInterfaceDeclaration)]] =
    for {
      httpErrorImports <- List(
        "org.asynchttpclient.Response"
      ).traverse(safeParseRawImport)
    } yield {
      def addStdConstructors(cls: ClassOrInterfaceDeclaration): Unit = {
        val _1 = cls
          .addConstructor(PUBLIC)
          .addParameter(new Parameter(new NodeList(finalModifier), STRING_TYPE, new SimpleName("message")))
          .setBody(
            new BlockStmt(
              new NodeList(
                new ExpressionStmt(new MethodCallExpr("super", new NameExpr("message")))
              )
            )
          )

        val _2 = cls
          .addConstructor(PUBLIC)
          .setParameters(
            new NodeList(
              new Parameter(new NodeList(finalModifier), STRING_TYPE, new SimpleName("message")),
              new Parameter(new NodeList(finalModifier), THROWABLE_TYPE, new SimpleName("cause"))
            )
          )
          .setBody(
            new BlockStmt(
              new NodeList(
                new ExpressionStmt(new MethodCallExpr("super", new NameExpr("message"), new NameExpr("cause")))
              )
            )
          )
      }

      def addNoSerialVersionUuid(cls: ClassOrInterfaceDeclaration): Unit = {
        val _ = cls.addSingleMemberAnnotation("SuppressWarnings", new StringLiteralExpr("serial"))
      }

      val clientExceptionClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier, abstractModifier), false, "ClientException")
        .addExtendedType("RuntimeException")
      addStdConstructors(clientExceptionClass)
      addNoSerialVersionUuid(clientExceptionClass)

      val marshallingExceptionClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier), false, "MarshallingException")
        .addExtendedType("ClientException")
      addStdConstructors(marshallingExceptionClass)
      addNoSerialVersionUuid(marshallingExceptionClass)

      val httpErrorClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier), false, "HttpError")
        .addExtendedType("ClientException")
      addNoSerialVersionUuid(httpErrorClass)
      val _1 = httpErrorClass.addField(RESPONSE_TYPE, "response", PRIVATE, FINAL)

      val _2 = httpErrorClass
        .addConstructor(PUBLIC)
        .addParameter(new Parameter(new NodeList(finalModifier), RESPONSE_TYPE, new SimpleName("response")))
        .setBody(
          new BlockStmt(
            new NodeList(
              new ExpressionStmt(
                new MethodCallExpr(
                  "super",
                  new BinaryExpr(
                    new StringLiteralExpr("HTTP server responded with status "),
                    new MethodCallExpr(new NameExpr("response"), "getStatusCode"),
                    BinaryExpr.Operator.PLUS
                  )
                )
              ),
              new ExpressionStmt(
                new AssignExpr(
                  new FieldAccessExpr(new ThisExpr, "response"),
                  new NameExpr("response"),
                  AssignExpr.Operator.ASSIGN
                )
              )
            )
          )
        )

      httpErrorClass
        .addMethod("getResponse", PUBLIC)
        .setType(RESPONSE_TYPE)
        .setBody(
          new BlockStmt(
            new NodeList(
              new ReturnStmt(new FieldAccessExpr(new ThisExpr, "response"))
            )
          )
        )

      List(
        (List.empty, clientExceptionClass),
        (List.empty, marshallingExceptionClass),
        (httpErrorImports, httpErrorClass)
      )
    }

  def generateAsyncHttpClientSupportClass(): Target[(List[ImportDeclaration], ClassOrInterfaceDeclaration)] =
    for {
      imports <- List(
        "java.util.concurrent.CompletionStage",
        "java.util.function.Function",
        "org.asynchttpclient.AsyncHttpClient",
        "org.asynchttpclient.AsyncHttpClientConfig",
        "org.asynchttpclient.DefaultAsyncHttpClient",
        "org.asynchttpclient.DefaultAsyncHttpClientConfig",
        "org.asynchttpclient.Request",
        "org.asynchttpclient.Response"
      ).traverse(safeParseRawImport)
    } yield {
      val cls = new ClassOrInterfaceDeclaration(new NodeList(publicModifier, abstractModifier), false, "AsyncHttpClientSupport")
      cls.addConstructor(PRIVATE)

      val ahcConfigBuilder = new ObjectCreationExpr(null, DEFAULT_ASYNC_HTTP_CLIENT_CONFIG_BUILDER_TYPE, new NodeList())
      val ahcConfig = new MethodCallExpr(
        List(
          ("setMaxRequestRetry", 2),
          ("setConnectTimeout", 3000),
          ("setRequestTimeout", 10000),
          ("setReadTimeout", 3000),
          ("setMaxConnections", 1024),
          ("setMaxConnectionsPerHost", 1024)
        ).foldLeft[Expression](ahcConfigBuilder)({
          case (lastExpr, (name, arg)) =>
            new MethodCallExpr(lastExpr, name, new NodeList[Expression](new IntegerLiteralExpr(arg)))
        }),
        "build",
        new NodeList[Expression]()
      )

      cls
        .addMethod("createDefaultAsyncHttpClient", PUBLIC, STATIC)
        .setType(ASYNC_HTTP_CLIENT_TYPE)
        .setBody(
          new BlockStmt(
            new NodeList(
              new ExpressionStmt(new VariableDeclarationExpr(new VariableDeclarator(ASYNC_HTTP_CLIENT_CONFIG_TYPE, "config", ahcConfig), finalModifier)),
              new ReturnStmt(
                new ObjectCreationExpr(null, DEFAULT_ASYNC_HTTP_CLIENT_TYPE, new NodeList(new NameExpr("config")))
              )
            )
          )
        )

      cls
        .addMethod("createHttpClient", PUBLIC, STATIC)
        .setType(HTTP_CLIENT_FUNCTION_TYPE)
        .addParameter(new Parameter(new NodeList(finalModifier), ASYNC_HTTP_CLIENT_TYPE, new SimpleName("client")))
        .setBody(
          new BlockStmt(
            new NodeList(
              new ReturnStmt(
                new LambdaExpr(
                  new NodeList(new Parameter(new NodeList(finalModifier), REQUEST_TYPE, new SimpleName("request"))),
                  new ExpressionStmt(
                    new MethodCallExpr(
                      new MethodCallExpr(new NameExpr("client"), "executeRequest", new NodeList[Expression](new NameExpr("request"))),
                      "toCompletableFuture"
                    )
                  ),
                  true
                )
              )
            )
          )
        )

      (imports, cls)
    }

  def generateJacksonSupportClass(): Target[(List[ImportDeclaration], ClassOrInterfaceDeclaration)] =
    for {
      imports <- List(
        "com.fasterxml.jackson.databind.ObjectMapper",
        "com.fasterxml.jackson.datatype.jdk8.Jdk8Module",
        "com.fasterxml.jackson.datatype.jsr310.JavaTimeModule"
      ).traverse(safeParseRawImport)
    } yield {
      val cls = new ClassOrInterfaceDeclaration(new NodeList(publicModifier, abstractModifier), false, "JacksonSupport")
      cls.addConstructor(PRIVATE)

      cls
        .addMethod("configureObjectMapper", PUBLIC, STATIC)
        .setType(OBJECT_MAPPER_TYPE)
        .addParameter(new Parameter(new NodeList(finalModifier), OBJECT_MAPPER_TYPE, new SimpleName("objectMapper")))
        .setBody(
          new BlockStmt(
            new NodeList(
              List("JavaTimeModule", "Jdk8Module").map(
                name =>
                  new ExpressionStmt(
                    new MethodCallExpr(
                      new NameExpr("objectMapper"),
                      "registerModule",
                      new NodeList[Expression](new ObjectCreationExpr(null, StaticJavaParser.parseClassOrInterfaceType(name), new NodeList()))
                    )
                  )
              ) :+
                  new ReturnStmt(new NameExpr("objectMapper")): _*
            )
          )
        )

      (imports, cls)
    }

  object ClientTermInterp extends (ClientTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ClientTerm[JavaLanguage, T]): Target[T] = term match {
      case GenerateClientOperation(
          _,
          RouteMeta(pathStr, httpMethod, operation, securityRequirements),
          methodName,
          tracing,
          parameters,
          responses,
          securitySchemes
          ) =>
        val responseParentName = s"${operation.get.getOperationId.capitalize}Response"
        val callBuilderName    = s"${operation.get.getOperationId.capitalize}CallBuilder"
        for {
          responseParentType <- safeParseClassOrInterfaceType(responseParentName)
          callBuilderType    <- safeParseClassOrInterfaceType(callBuilderName)

          pathExprNode <- SwaggerUtil.paths.generateUrlPathParams[JavaLanguage](
            pathStr,
            parameters.pathParams,
            new StringLiteralExpr(_),
            name =>
              new MethodCallExpr(
                new MethodCallExpr(new NameExpr("Shower"), "getInstance"),
                "show",
                new NodeList[Expression](new NameExpr(name.asString))
              ),
            new MethodCallExpr(new FieldAccessExpr(new ThisExpr, "baseUrl"), "toString"),
            (a, b) =>
              (a, b) match {
                case (ae: Expression, be: Expression) => new BinaryExpr(ae, be, BinaryExpr.Operator.PLUS)
                case _                                => a
              }
          )

          pathExpr <- pathExprNode match {
            case e: Expression => Target.pure(e)
            case x =>
              Target.raiseUserError[Expression](s"BUG: Returned node from generateUrlPathParams() was a ${x.getClass.getName}, not an Expression as expected")
          }
        } yield {
          val method = new MethodDeclaration(new NodeList(publicModifier), new VoidType, methodName)
            .setType(callBuilderType)

          parameters.parameters.foreach(p => p.param.setType(p.param.getType.unbox))
          (
            parameters.pathParams ++
                parameters.queryStringParams ++
                parameters.formParams ++
                parameters.headerParams ++
                parameters.bodyParams
          ).filter(_.required).map(_.param).foreach(method.addParameter)

          val requestBuilder = new MethodCallExpr(
            new AssignExpr(
              new VariableDeclarationExpr(new VariableDeclarator(REQUEST_BUILDER_TYPE, "builder"), finalModifier),
              new ObjectCreationExpr(
                null,
                REQUEST_BUILDER_TYPE,
                new NodeList[Expression](
                  new StringLiteralExpr(httpMethod.toString)
                )
              ),
              AssignExpr.Operator.ASSIGN
            ),
            "setUrl",
            new NodeList[Expression](pathExpr)
          )

          val builderParamsMethodNames = List(
            (parameters.queryStringParams, "addQueryParam", false),
            (parameters.formParams, "addFormParam", parameters.formParams.exists(_.isFile)),
            (parameters.headerParams, "addHeader", false)
          )

          val allConsumes = operation.get.consumes.flatMap(ContentType.unapply).toList
          val consumes    = DropwizardHelpers.getBestConsumes(operation, allConsumes, parameters)
          val allProduces = operation.get.produces.flatMap(ContentType.unapply).toList
          val produces =
            responses.value.map(resp => (resp.statusCode, DropwizardHelpers.getBestProduces(operation.get.getOperationId, allProduces, resp))).toMap

          val builderMethodCalls: List[(ScalaParameter[JavaLanguage], Statement)] = builderParamsMethodNames
              .flatMap({
                case (params, name, needsMultipart) =>
                  params.map(param => (param, generateBuilderMethodCall(param, name, needsMultipart)))
              }) ++
                parameters.bodyParams.flatMap(param => generateBodyMethodCall(param, consumes).map(stmt => (param, stmt)))

          val callBuilderCreation = new ObjectCreationExpr(
            null,
            callBuilderType,
            (List[Expression](
              new NameExpr("builder"),
              new FieldAccessExpr(new ThisExpr, "httpClient"),
              new FieldAccessExpr(new ThisExpr, "objectMapper")
            ) ++ (if (tracing) Option(new FieldAccessExpr(new ThisExpr, "clientName")) else None)).toNodeList
          )

          method.setBody(
            new BlockStmt(
              new NodeList(
                new ExpressionStmt(requestBuilder) +:
                    builderMethodCalls.filter(_._1.required).map(_._2) :+
                    new ReturnStmt(callBuilderCreation): _*
              )
            )
          )

          val callBuilderFinalFields = List(
              (REQUEST_BUILDER_TYPE, "builder"),
              (HTTP_CLIENT_FUNCTION_TYPE, "httpClient"),
              (OBJECT_MAPPER_TYPE, "objectMapper")
            ) ++ (if (tracing) Option((STRING_TYPE, "clientName")) else None)

          val callBuilderCls = new ClassOrInterfaceDeclaration(new NodeList(publicModifier, staticModifier), false, callBuilderName)
          callBuilderFinalFields.foreach({ case (tpe, name) => callBuilderCls.addField(tpe, name, PRIVATE, FINAL) })
          val callBuilderInitContentType = consumes.map({ ct =>
            val ctStr = ct match {
              case TextContent(_) => s"${ct.value}; charset=utf-8"
              case _              => ct.value
            }
            new ExpressionStmt(
              new MethodCallExpr(
                "withHeader",
                new StringLiteralExpr("Content-Type"),
                new StringLiteralExpr(ctStr)
              )
            )
          })
          val callBuilderInitAccept = {
            val acceptHeaderString = produces
              .flatMap({ case (_, ct) => ct.map(ct => s"${ct.value}; q=1.0") })
              .mkString(", ")
            if (acceptHeaderString.nonEmpty) {
              Option(
                new ExpressionStmt(
                  new MethodCallExpr(
                    "withHeader",
                    new StringLiteralExpr("Accept"),
                    new StringLiteralExpr(s"$acceptHeaderString, */*; q=0.1")
                  )
                )
              )
            } else {
              Option.empty
            }
          }

          callBuilderCls
            .addConstructor(PRIVATE)
            .setParameters(
              callBuilderFinalFields.map({ case (tpe, name) => new Parameter(new NodeList(finalModifier), tpe, new SimpleName(name)) }).toNodeList
            )
            .setBody(
              new BlockStmt(
                (
                  callBuilderFinalFields
                    .map[Statement, List[Statement]]({
                      case (_, name) =>
                        new ExpressionStmt(new AssignExpr(new FieldAccessExpr(new ThisExpr, name), new NameExpr(name), AssignExpr.Operator.ASSIGN))
                    }) ++ callBuilderInitContentType ++ callBuilderInitAccept
                ).toNodeList
              )
            )

          val optionalParamMethods = builderMethodCalls
            .filterNot(_._1.required)
            .flatMap({
              case (ScalaParameter(_, param, _, _, argType), methodCall) =>
                val containedType = argType.containedType.unbox

                val optionalOverrideMethod = if (argType.isOptional && !containedType.isOptional) {
                  val methodParamName = s"optional${param.getNameAsString.capitalize}"

                  val lambdaBody = methodCall match {
                    case es: ExpressionStmt => es.clone()
                    case stmt               => new BlockStmt(new NodeList(stmt.clone()))
                  }

                  Some(
                    new MethodDeclaration(
                      new NodeList(publicModifier),
                      s"with${param.getNameAsString.unescapeIdentifier.capitalize}",
                      callBuilderType,
                      List(
                        new Parameter(new NodeList(finalModifier), argType, new SimpleName(methodParamName))
                      ).toNodeList
                    ).setBody(
                      new BlockStmt(
                        List(
                          new ExpressionStmt(
                            new MethodCallExpr(
                              new NameExpr(methodParamName),
                              "ifPresent",
                              new NodeList[Expression](
                                new LambdaExpr(
                                  new NodeList(new Parameter(new UnknownType, param.getNameAsString)),
                                  lambdaBody,
                                  false
                                )
                              )
                            )
                          ),
                          new ReturnStmt(new ThisExpr)
                        ).toNodeList
                      )
                    )
                  )
                } else {
                  Option.empty[MethodDeclaration]
                }

                val mainMethod = new MethodDeclaration(
                  new NodeList(publicModifier),
                  s"with${param.getNameAsString.unescapeIdentifier.capitalize}",
                  callBuilderType,
                  List(
                    new Parameter(new NodeList(finalModifier), containedType, new SimpleName(param.getNameAsString))
                  ).toNodeList
                ).setBody(
                  new BlockStmt(
                    List(
                      methodCall,
                      new ReturnStmt(new ThisExpr)
                    ).toNodeList
                  )
                )

                mainMethod +: optionalOverrideMethod.toList
            })
          optionalParamMethods.foreach(callBuilderCls.addMember)

          callBuilderCls
            .addMethod("withHeader", PUBLIC)
            .setParameters(
              List(
                new Parameter(new NodeList(finalModifier), STRING_TYPE, new SimpleName("name")),
                new Parameter(new NodeList(finalModifier), STRING_TYPE, new SimpleName("value"))
              ).toNodeList
            )
            .setType(callBuilderType)
            .setBody(
              new BlockStmt(
                List(
                  new ExpressionStmt(
                    new MethodCallExpr(
                      new FieldAccessExpr(new ThisExpr, "builder"),
                      "addHeader",
                      List[Expression](new NameExpr("name"), new NameExpr("value")).toNodeList
                    )
                  ),
                  new ReturnStmt(new ThisExpr)
                ).toNodeList
              )
            )

          val httpMethodCallExpr = new MethodCallExpr(
            new FieldAccessExpr(new ThisExpr, "httpClient"),
            "apply",
            new NodeList[Expression](new MethodCallExpr(new NameExpr("builder"), "build"))
          )
          val requestCall = new MethodCallExpr(
            httpMethodCallExpr,
            "thenApply",
            new NodeList[Expression](
              new LambdaExpr(
                new NodeList(new Parameter(new NodeList(finalModifier), RESPONSE_TYPE, new SimpleName("response"))),
                new BlockStmt(
                  new NodeList(
                    new SwitchStmt(
                      new MethodCallExpr(new NameExpr("response"), "getStatusCode"),
                      new NodeList(
                        responses.value.map(
                          response =>
                            new SwitchEntry(
                              new NodeList(new IntegerLiteralExpr(response.statusCode)),
                              SwitchEntry.Type.BLOCK,
                              new NodeList(response.value match {
                                case None =>
                                  new ReturnStmt(
                                    new ObjectCreationExpr(
                                      null,
                                      StaticJavaParser.parseClassOrInterfaceType(s"${responseParentName}.${response.statusCodeName.asString}"),
                                      new NodeList()
                                    )
                                  )
                                case Some((valueType, _)) =>
                                  new TryStmt(
                                    new BlockStmt(
                                      new NodeList(
                                        new ExpressionStmt(
                                          produces
                                            .get(response.statusCode)
                                            .flatten
                                            .getOrElse({
                                              println(
                                                s"WARNING: no supported content type specified at ${operation.showHistory}'s ${response.statusCode} response; falling back to application/json"
                                              )
                                              ApplicationJson
                                            }) match {
                                            case ApplicationJson =>
                                              new AssignExpr(
                                                new VariableDeclarationExpr(new VariableDeclarator(valueType, "result"), finalModifier),
                                                new MethodCallExpr(
                                                  new FieldAccessExpr(new ThisExpr, "objectMapper"),
                                                  "readValue",
                                                  new NodeList[Expression](
                                                    new MethodCallExpr(new NameExpr("response"), "getResponseBodyAsStream"),
                                                    jacksonTypeReferenceFor(valueType)
                                                  )
                                                ),
                                                AssignExpr.Operator.ASSIGN
                                              )

                                            case UrlencodedFormData =>
                                              new AssignExpr(
                                                new VariableDeclarationExpr(new VariableDeclarator(valueType, "result"), finalModifier),
                                                new MethodCallExpr(
                                                  new NameExpr("GuardrailJacksonFormUtils"),
                                                  "readUrlEncodedForm",
                                                  new NodeList[Expression](
                                                    new FieldAccessExpr(new ThisExpr, "objectMapper"),
                                                    new ClassExpr(valueType),
                                                    new LambdaExpr(
                                                      new NodeList[Parameter],
                                                      new ExpressionStmt(
                                                        new MethodCallExpr(
                                                          new NameExpr("AsyncHttpClientUtils"),
                                                          "parseFormData",
                                                          new NodeList[Expression](new NameExpr("response"))
                                                        )
                                                      ),
                                                      true
                                                    )
                                                  )
                                                ),
                                                AssignExpr.Operator.ASSIGN
                                              )

                                            case contentType =>
                                              val bodyGetter = valueType match {
                                                case _ if valueType.isNamed("InputStream") => "getResponseBodyAsStream"
                                                case _ if valueType.isNamed("byte[]")      => "getResponseBodyAsBytes"
                                                case _ if valueType.isNamed("ByteBuffer")  => "getResponseBodyAsByteBuffer"
                                                case _ if valueType.isNamed("String")      => "getResponseBody"
                                                case _ =>
                                                  println(
                                                    s"WARNING: Don't know how to handle response of type ${valueType.asString} for content type $contentType at ${operation.showHistory}; falling back to String"
                                                  )
                                                  "getResponseBody"
                                              }
                                              val bodyGetterExpr = new MethodCallExpr(
                                                new NameExpr("response"),
                                                bodyGetter,
                                                bodyGetter match {
                                                  case "getResponseBody" =>
                                                    new NodeList[Expression](
                                                      new MethodCallExpr(
                                                        new MethodCallExpr(
                                                          new NameExpr("AsyncHttpClientUtils"),
                                                          "getResponseCharset",
                                                          new NodeList[Expression](new NameExpr("response"))
                                                        ),
                                                        "orElse",
                                                        new NodeList[Expression](new FieldAccessExpr(new NameExpr("StandardCharsets"), "UTF_8"))
                                                      )
                                                    )
                                                  case _ => new NodeList[Expression]
                                                }
                                              )
                                              new AssignExpr(
                                                new VariableDeclarationExpr(new VariableDeclarator(valueType, "result"), finalModifier),
                                                bodyGetterExpr,
                                                AssignExpr.Operator.ASSIGN
                                              )
                                          }
                                        ),
                                        new IfStmt(
                                          new BinaryExpr(new NameExpr("result"), new NullLiteralExpr, BinaryExpr.Operator.EQUALS),
                                          new BlockStmt(
                                            new NodeList(
                                              new ThrowStmt(
                                                new ObjectCreationExpr(
                                                  null,
                                                  MARSHALLING_EXCEPTION_TYPE,
                                                  new NodeList(new StringLiteralExpr("Failed to unmarshal response"))
                                                )
                                              )
                                            )
                                          ),
                                          new BlockStmt(
                                            new NodeList(
                                              new ReturnStmt(
                                                new ObjectCreationExpr(
                                                  null,
                                                  StaticJavaParser.parseClassOrInterfaceType(s"${responseParentName}.${response.statusCodeName.asString}"),
                                                  new NodeList[Expression](new NameExpr("result"))
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    ),
                                    new NodeList(
                                      new CatchClause(
                                        new Parameter(new NodeList(finalModifier), MARSHALLING_EXCEPTION_TYPE, new SimpleName("e")),
                                        new BlockStmt(
                                          new NodeList(
                                            new ThrowStmt(new NameExpr("e"))
                                          )
                                        )
                                      ),
                                      new CatchClause(
                                        new Parameter(new NodeList(finalModifier), EXCEPTION_TYPE, new SimpleName("e")),
                                        new BlockStmt(
                                          new NodeList(
                                            new ThrowStmt(
                                              new ObjectCreationExpr(
                                                null,
                                                MARSHALLING_EXCEPTION_TYPE,
                                                new NodeList(new MethodCallExpr(new NameExpr("e"), "getMessage"), new NameExpr("e"))
                                              )
                                            )
                                          )
                                        )
                                      )
                                    ),
                                    null
                                  )
                              })
                            )
                        ) :+ new SwitchEntry(
                              new NodeList(),
                              SwitchEntry.Type.BLOCK,
                              new NodeList(new ThrowStmt(new ObjectCreationExpr(null, HTTP_ERROR_TYPE, new NodeList(new NameExpr("response")))))
                            ): _*
                      )
                    )
                  )
                ),
                true
              )
            )
          )

          callBuilderCls
            .addMethod("call", PUBLIC)
            .setType(completionStageType(responseParentType))
            .addThrownException(CLIENT_EXCEPTION_TYPE)
            .setBody(new BlockStmt(List[Statement](new ReturnStmt(requestCall)).toNodeList))

          RenderedClientOperation[JavaLanguage](method, callBuilderCls :: Nil)
        }

      case GetImports(tracing) =>
        if (tracing) {
          Target.raiseUserError("Tracing is not yet supported by this framework")
        } else {
          (List(
            "java.net.URI",
            "java.nio.charset.StandardCharsets",
            "java.util.Optional",
            "java.util.concurrent.CompletionStage",
            "java.util.function.Function",
            "java.util.function.Supplier",
            "com.fasterxml.jackson.core.JsonProcessingException",
            "com.fasterxml.jackson.core.type.TypeReference",
            "com.fasterxml.jackson.databind.ObjectMapper",
            "org.asynchttpclient.Request",
            "org.asynchttpclient.RequestBuilder",
            "org.asynchttpclient.Response",
            "org.asynchttpclient.request.body.multipart.InputStreamPart",
            "org.asynchttpclient.request.body.multipart.StringPart"
          ).map(safeParseRawImport) ++ List(
                "java.util.Objects.requireNonNull"
              ).map(safeParseRawStaticImport)).sequence
        }

      case GetExtraImports(tracing) =>
        Target.pure(List.empty)

      case ClientClsArgs(tracingName, serverUrls, tracing) =>
        Target.pure(List.empty)

      case GenerateResponseDefinitions(operationId, responses, protocolElems) =>
        val abstractClassName = s"${operationId.capitalize}Response"
        val genericTypeParam  = StaticJavaParser.parseClassOrInterfaceType("T")

        val responseData = responses.value.map({
          case Response(statusCodeName, valueType, _) =>
            val responseName: String = statusCodeName.asString
            val responseType         = StaticJavaParser.parseClassOrInterfaceType(responseName)
            val responseLambdaName   = s"handle${responseName}"

            val responseInnerClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier, staticModifier), false, responseName);
            responseInnerClass.addExtendedType(abstractClassName)
            val (foldMethodParamType, foldMethodApplier, foldMethodArgs) = valueType.fold(
              (supplierType(genericTypeParam), "get", new NodeList[Expression]())
            )({
              vt =>
                val finalValueType: Type = vt.unbox

                responseInnerClass.addField(finalValueType, "value", PRIVATE, FINAL)

                val constructor = responseInnerClass.addConstructor(PUBLIC)
                constructor.addParameter(new Parameter(new NodeList(finalModifier), finalValueType, new SimpleName("value")))
                constructor.setBody(
                  new BlockStmt(
                    new NodeList(
                      new ExpressionStmt(
                        new AssignExpr(
                          new FieldAccessExpr(new ThisExpr, "value"),
                          new NameExpr("value"),
                          AssignExpr.Operator.ASSIGN
                        )
                      )
                    )
                  )
                )

                val getValueMethod = responseInnerClass.addMethod("getValue", PUBLIC)
                getValueMethod.setType(finalValueType)
                getValueMethod.setBody(
                  new BlockStmt(
                    new NodeList(
                      new ReturnStmt(new FieldAccessExpr(new ThisExpr, "value"))
                    )
                  )
                )

                (
                  functionType(vt, genericTypeParam),
                  "apply",
                  new NodeList[Expression](
                    new MethodCallExpr(new EnclosedExpr(new CastExpr(responseType, new ThisExpr)), "getValue")
                  )
                )
            })

            val foldMethodParameter = new Parameter(new NodeList(finalModifier), foldMethodParamType, new SimpleName(responseLambdaName))

            val foldMethodBranch = new IfStmt(
              new InstanceOfExpr(new ThisExpr, responseType),
              new BlockStmt(
                new NodeList(
                  new ReturnStmt(
                    new MethodCallExpr(
                      new NameExpr(responseLambdaName),
                      foldMethodApplier,
                      foldMethodArgs
                    )
                  )
                )
              ),
              null
            )

            (responseInnerClass, foldMethodParameter, foldMethodBranch)
        })

        val abstractResponseClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier, abstractModifier), false, abstractClassName)

        val (innerClasses, foldMethodParameters, foldMethodIfBranches) = responseData.unzip3

        innerClasses.foreach(abstractResponseClass.addMember)

        val foldMethod = abstractResponseClass
          .addMethod("fold", PUBLIC)
          .addTypeParameter("T")
          .setType("T")
        foldMethodParameters.foreach(foldMethod.addParameter)

        NonEmptyList
          .fromList(foldMethodIfBranches)
          .foreach({ nel =>
            nel.reduceLeft({ (prev, next) =>
              prev.setElseStmt(next)
              next
            })

            nel.last.setElseStmt(
              new BlockStmt(
                new NodeList(
                  new ThrowStmt(new ObjectCreationExpr(null, ASSERTION_ERROR_TYPE, new NodeList(new StringLiteralExpr("This is a bug in guardrail!"))))
                )
              )
            )

            foldMethod.setBody(new BlockStmt(new NodeList(nel.head)))
          })

        Target.pure(List(abstractResponseClass))

      case GenerateSupportDefinitions(tracing, securitySchemes) =>
        for {
          exceptionClasses <- generateClientExceptionClasses()
          ahcSupport       <- generateAsyncHttpClientSupportClass()
          (ahcSupportImports, ahcSupportClass) = ahcSupport
          jacksonSupport <- generateJacksonSupportClass()
          (jacksonSupportImports, jacksonSupportClass) = jacksonSupport
          asyncHttpclientUtils <- asyncHttpClientUtilsSupportDef
          jacksonFormUtils     <- SerializationHelpers.jacksonFormUtils
          shower               <- SerializationHelpers.showerSupportDef
        } yield {
          exceptionClasses.map({
            case (imports, cls) =>
              SupportDefinition[JavaLanguage](new Name(cls.getNameAsString), imports, cls)
          }) ++ List(
            SupportDefinition[JavaLanguage](new Name(ahcSupportClass.getNameAsString), ahcSupportImports, ahcSupportClass),
            SupportDefinition[JavaLanguage](new Name(jacksonSupportClass.getNameAsString), jacksonSupportImports, jacksonSupportClass),
            asyncHttpclientUtils,
            jacksonFormUtils,
            shower
          )
        }

      case BuildStaticDefns(clientName, tracingName, serverUrls, ctorArgs, tracing) =>
        Target.pure(
          StaticDefns[JavaLanguage](
            className = clientName,
            extraImports = List.empty,
            definitions = List.empty
          )
        )

      case BuildClient(clientName, tracingName, serverUrls, basePath, ctorArgs, clientCalls, supportDefinitions, tracing) =>
        val clientType = StaticJavaParser.parseClassOrInterfaceType(clientName)
        val serverUrl  = serverUrls.map(_.head).map(uri => new URI(uri.toString + basePath.getOrElse("")))

        val (baseUrlField, defaultBaseUrlField) = {
          val (modifiers, declarator) = serverUrl.fold(
            (new NodeList(privateModifier, finalModifier), new VariableDeclarator(URI_TYPE, "baseUrl"))
          )(
            _ => (new NodeList(privateModifier), new VariableDeclarator(URI_TYPE, "baseUrl", new NameExpr("DEFAULT_BASE_URL")))
          )
          (
            new FieldDeclaration(modifiers, declarator),
            serverUrl.map({ serverUrl =>
              new FieldDeclaration(
                new NodeList(privateModifier, staticModifier, finalModifier),
                new VariableDeclarator(
                  URI_TYPE,
                  "DEFAULT_BASE_URL",
                  new MethodCallExpr(
                    new NameExpr("URI"),
                    "create",
                    new NodeList[Expression](new StringLiteralExpr(serverUrl.toString))
                  )
                )
              )
            })
          )
        }

        val tracingFields = if (tracing) {
          val (modifiers, declarator) = tracingName.fold(
            (new NodeList(privateModifier, finalModifier), new VariableDeclarator(STRING_TYPE, "clientName"))
          )(
            _ => (new NodeList(privateModifier), new VariableDeclarator(STRING_TYPE, "clientName", new NameExpr("DEFAULT_CLIENT_NAME")))
          )
          val clientNameField = new FieldDeclaration(modifiers, declarator)
          val defaultClientNameField = tracingName.map({ tracingName =>
            new FieldDeclaration(
              new NodeList(privateModifier, staticModifier, finalModifier),
              new VariableDeclarator(
                STRING_TYPE,
                "DEFAULT_CLIENT_NAME",
                new StringLiteralExpr(tracingName)
              )
            )
          })

          defaultClientNameField.toList :+ clientNameField
        } else {
          List.empty
        }

        val httpClientField = new FieldDeclaration(
          new NodeList(privateModifier),
          new VariableDeclarator(
            optionalType(HTTP_CLIENT_FUNCTION_TYPE),
            "httpClient",
            new MethodCallExpr(new NameExpr("Optional"), "empty")
          )
        )
        val objectMapperField = new FieldDeclaration(
          new NodeList(privateModifier),
          new VariableDeclarator(
            optionalType(OBJECT_MAPPER_TYPE),
            "objectMapper",
            new MethodCallExpr(new NameExpr("Optional"), "empty")
          )
        )

        val builderConstructor = new ConstructorDeclaration(new NodeList(publicModifier), "Builder")
        def createConstructorParameter(tpe: Type, name: String): Parameter =
          new Parameter(new NodeList(finalModifier), tpe, new SimpleName(name))
        def createBuilderConstructorAssignment(name: String): Statement =
          new ExpressionStmt(
            new AssignExpr(new FieldAccessExpr(new ThisExpr, name), requireNonNullExpr(name), AssignExpr.Operator.ASSIGN)
          )
        (serverUrl, tracingName) match {
          case (None, None) if tracing =>
            builderConstructor.setParameters(
              new NodeList(
                createConstructorParameter(URI_TYPE, "baseUrl"),
                createConstructorParameter(STRING_TYPE, "clientName")
              )
            )
            builderConstructor.setBody(
              new BlockStmt(
                new NodeList(
                  createBuilderConstructorAssignment("baseUrl"),
                  createBuilderConstructorAssignment("clientName")
                )
              )
            )

          case (Some(_), None) if tracing =>
            builderConstructor.setParameters(
              new NodeList(
                createConstructorParameter(STRING_TYPE, "clientName")
              )
            )
            builderConstructor.setBody(
              new BlockStmt(
                new NodeList(
                  createBuilderConstructorAssignment("clientName")
                )
              )
            )

          case (None, _) =>
            builderConstructor.setParameters(
              new NodeList(
                createConstructorParameter(URI_TYPE, "baseUrl")
              )
            )
            builderConstructor.setBody(
              new BlockStmt(
                new NodeList(
                  createBuilderConstructorAssignment("baseUrl")
                )
              )
            )

          case (Some(_), Some(_)) => // no params

          case (Some(_), _) if !tracing => // no params
        }

        def createSetter(tpe: Type, name: String, initializer: String => Expression): MethodDeclaration =
          new MethodDeclaration(new NodeList(publicModifier), BUILDER_TYPE, s"with${name.capitalize}")
            .addParameter(new Parameter(new NodeList(finalModifier), tpe, new SimpleName(name)))
            .setBody(
              new BlockStmt(
                new NodeList(
                  new ExpressionStmt(new AssignExpr(new FieldAccessExpr(new ThisExpr, name), initializer(name), AssignExpr.Operator.ASSIGN)),
                  new ReturnStmt(new ThisExpr)
                )
              )
            )
        val nonNullInitializer: String => Expression                                  = name => requireNonNullExpr(name)
        def optionalInitializer(valueArg: String => Expression): String => Expression = name => optionalOfExpr(valueArg(name))

        val builderSetters = List(
          if (serverUrl.isDefined) Some(createSetter(URI_TYPE, "baseUrl", nonNullInitializer)) else None,
          if (tracing && tracingName.isDefined) Some(createSetter(STRING_TYPE, "clientName", nonNullInitializer)) else None,
          Some(createSetter(HTTP_CLIENT_FUNCTION_TYPE, "httpClient", optionalInitializer(new NameExpr(_)))),
          Some(
            createSetter(
              OBJECT_MAPPER_TYPE,
              "objectMapper",
              optionalInitializer(
                name => new MethodCallExpr(new NameExpr("JacksonSupport"), "configureObjectMapper", new NodeList[Expression](new NameExpr(name)))
              )
            )
          )
        ).flatten

        val buildMethod = new MethodDeclaration(new NodeList(publicModifier), clientType, "build")
          .setBody(
            new BlockStmt(
              new NodeList(
                new ReturnStmt(new ObjectCreationExpr(null, clientType, new NodeList(new ThisExpr)))
              )
            )
          )

        def createInternalGetter(tpe: Type, name: String, getterCall: Expression): MethodDeclaration =
          new MethodDeclaration(new NodeList(privateModifier), tpe, s"get${name.capitalize}")
            .setBody(
              new BlockStmt(
                new NodeList(
                  new ReturnStmt(
                    new MethodCallExpr(
                      new FieldAccessExpr(new ThisExpr, name),
                      "orElseGet",
                      new NodeList[Expression](
                        new LambdaExpr(new NodeList(), new ExpressionStmt(getterCall), true)
                      )
                    )
                  )
                )
              )
            )
        val internalGetters = List(
          createInternalGetter(
            HTTP_CLIENT_FUNCTION_TYPE,
            "httpClient",
            new MethodCallExpr(
              new NameExpr("AsyncHttpClientSupport"),
              "createHttpClient",
              new NodeList[Expression](
                new MethodCallExpr(new NameExpr("AsyncHttpClientSupport"), "createDefaultAsyncHttpClient")
              )
            )
          ),
          createInternalGetter(
            OBJECT_MAPPER_TYPE,
            "objectMapper",
            new MethodCallExpr(
              new NameExpr("JacksonSupport"),
              "configureObjectMapper",
              new NodeList[Expression](new ObjectCreationExpr(null, OBJECT_MAPPER_TYPE, new NodeList()))
            )
          )
        )

        val builderClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier, staticModifier), false, "Builder")
        sortDefinitions(
          tracingFields ++
              builderSetters ++
              internalGetters ++
              defaultBaseUrlField ++
              List(
                baseUrlField,
                httpClientField,
                objectMapperField,
                builderConstructor,
                buildMethod
              )
        ).foreach(builderClass.addMember)

        val clientFields = List(
          Some((URI_TYPE, "baseUrl")),
          if (tracing) Some((STRING_TYPE, "clientName")) else None,
          Some((HTTP_CLIENT_FUNCTION_TYPE, "httpClient")),
          Some((OBJECT_MAPPER_TYPE, "objectMapper"))
        ).flatten.map({
          case (tpe, name) =>
            new FieldDeclaration(new NodeList(privateModifier, finalModifier), new VariableDeclarator(tpe, name))
        })

        val constructor = new ConstructorDeclaration(new NodeList(privateModifier), clientName)
        constructor.addParameter(new Parameter(new NodeList(finalModifier), BUILDER_TYPE, new SimpleName("builder")))
        def newFieldAccessExpr(scope: Expression, name: String): Expression = new FieldAccessExpr(scope, name)
        def newMethodCallExpr(scope: Expression, name: String): Expression  = new MethodCallExpr(scope, s"get${name.capitalize}")
        constructor.setBody(
          new BlockStmt(
            new NodeList(
              List[Option[(String, (Expression, String) => Expression)]](
                Some(("baseUrl", newFieldAccessExpr)),
                if (tracing) Some(("clientName", newFieldAccessExpr)) else None,
                Some(("httpClient", newMethodCallExpr)),
                Some(("objectMapper", newMethodCallExpr))
              ).flatten.map({
                case (name, value) =>
                  new ExpressionStmt(
                    new AssignExpr(
                      new FieldAccessExpr(new ThisExpr, name),
                      value(new NameExpr("builder"), name),
                      AssignExpr.Operator.ASSIGN
                    )
                  )
              }): _*
            )
          )
        )

        val clientClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier), false, clientName)
        sortDefinitions(
          List(
            builderClass,
            constructor
          ) ++
              clientCalls ++
              clientFields ++
              supportDefinitions
        ).foreach(clientClass.addMember)

        Target.pure(NonEmptyList(Right(clientClass), Nil))
    }
  }
}
