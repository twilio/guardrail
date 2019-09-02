package com.twilio.guardrail.generators.Java

import cats.data.NonEmptyList
import cats.~>
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import com.github.javaparser.JavaParser
import com.github.javaparser.ast.Modifier._
import com.github.javaparser.ast.{ Modifier, Node, NodeList }
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, PrimitiveType, Type, VoidType }
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import com.twilio.guardrail.{ ADT, ClassDefinition, EnumDefinition, RandomType, RenderedRoutes, StrictProtocolElems, SupportDefinition, Target }
import com.twilio.guardrail.extract.ServerRawResponse
import com.twilio.guardrail.generators.ScalaParameters
import com.twilio.guardrail.generators.syntax.Java._
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.protocol.terms.Response
import com.twilio.guardrail.protocol.terms.server._
import com.twilio.guardrail.shims.OperationExt
import com.twilio.guardrail.terms.RouteMeta
import io.swagger.v3.oas.models.responses.ApiResponse
import java.util
import scala.collection.JavaConverters._
import scala.language.existentials
import scala.util.Try

object DropwizardServerGenerator {
  private implicit class ContentTypeExt(val ct: RouteMeta.ContentType) extends AnyVal {
    def toJaxRsAnnotationName: String = ct match {
      case RouteMeta.ApplicationJson    => "APPLICATION_JSON"
      case RouteMeta.UrlencodedFormData => "APPLICATION_FORM_URLENCODED"
      case RouteMeta.MultipartFormData  => "MULTIPART_FORM_DATA"
      case RouteMeta.TextPlain          => "TEXT_PLAIN"
      case RouteMeta.OctetStream        => "APPLICATION_OCTET_STREAM"
    }
  }

  private val ASYNC_RESPONSE_TYPE   = JavaParser.parseClassOrInterfaceType("AsyncResponse")
  private val RESPONSE_TYPE         = JavaParser.parseClassOrInterfaceType("Response")
  private val RESPONSE_BUILDER_TYPE = JavaParser.parseClassOrInterfaceType("Response.ResponseBuilder")
  private val LOGGER_TYPE           = JavaParser.parseClassOrInterfaceType("Logger")

  private def removeEmpty(s: String): Option[String]       = if (s.trim.isEmpty) None else Some(s.trim)
  private def splitPathComponents(s: String): List[String] = s.split("/").flatMap(removeEmpty).toList

  private def findPathPrefix(routePaths: List[String]): List[String] = {
    def getHeads(sss: List[List[String]]): (List[Option[String]], List[List[String]]) =
      (sss.map(_.headOption), sss.map(ss => ss.headOption.fold(List.empty[String])(_ => ss.tail)))

    def checkMatch(matching: List[String], headsToCheck: List[Option[String]], restOfHeads: List[List[String]]): List[String] =
      headsToCheck.headOption
        .flatMap({ firstO =>
          firstO.map({ first =>
            if (headsToCheck.tail.forall(_.contains(first))) {
              val (nextHeads, nextRest) = getHeads(restOfHeads)
              checkMatch(matching :+ first, nextHeads, nextRest)
            } else {
              matching
            }
          })
        })
        .getOrElse(matching)

    val splitRoutePaths             = routePaths.map(splitPathComponents)
    val (initialHeads, initialRest) = getHeads(splitRoutePaths)
    checkMatch(List.empty, initialHeads, initialRest)
  }

  // FIXME: does this handle includes from other files?
  private def definitionName(refName: Option[String]): Option[String] =
    refName.flatMap({ rn =>
      rn.split("/").toList match {
        case "#" :: _ :: name :: Nil => Some(name)
        case _                       => None
      }
    })

  def getBestConsumes(contentTypes: List[RouteMeta.ContentType], parameters: ScalaParameters[JavaLanguage]): Option[RouteMeta.ContentType] = {
    val priorityOrder = NonEmptyList.of(
      RouteMeta.UrlencodedFormData,
      RouteMeta.ApplicationJson,
      RouteMeta.MultipartFormData,
      RouteMeta.TextPlain
    )

    priorityOrder
      .foldLeft[Option[RouteMeta.ContentType]](None)({
        case (s @ Some(_), _) => s
        case (None, next)     => contentTypes.find(_ == next)
      })
      .orElse(parameters.formParams.headOption.map(_ => RouteMeta.UrlencodedFormData))
      .orElse(parameters.bodyParams.map(_ => RouteMeta.ApplicationJson))
  }

  private def getBestProduces(contentTypes: List[RouteMeta.ContentType],
                              responses: List[ApiResponse],
                              protocolElems: List[StrictProtocolElems[JavaLanguage]]): Option[RouteMeta.ContentType] = {
    val priorityOrder = NonEmptyList.of(
      RouteMeta.ApplicationJson,
      RouteMeta.TextPlain,
      RouteMeta.OctetStream
    )

    priorityOrder
      .foldLeft[Option[RouteMeta.ContentType]](None)({
        case (s @ Some(_), _) => s
        case (None, next)     => contentTypes.find(_ == next)
      })
      .orElse(
        responses
          .map({ resp =>
            protocolElems
              .find(pe => definitionName(Option(resp.get$ref())).contains(pe.name))
              .flatMap({
                case _: ClassDefinition[_]                                              => Some(RouteMeta.ApplicationJson)
                case RandomType(_, tpe) if tpe.isPrimitiveType || tpe.isNamed("String") => Some(RouteMeta.TextPlain)
                case _: ADT[_] | _: EnumDefinition[_]                                   => Some(RouteMeta.TextPlain)
                case _                                                                  => None
              })
          })
          .headOption
          .flatten
      )
  }

  def generateResponseSuperClass(name: String): Target[ClassOrInterfaceDeclaration] = {
    val cls = new ClassOrInterfaceDeclaration(util.EnumSet.of(ABSTRACT), false, name)
    cls.addField(PrimitiveType.intType, "statusCode", PRIVATE, FINAL)

    cls
      .addConstructor()
      .addParameter(new Parameter(util.EnumSet.of(FINAL), PrimitiveType.intType, new SimpleName("statusCode")))
      .setBody(
        new BlockStmt(
          new NodeList(
            new ExpressionStmt(new AssignExpr(new FieldAccessExpr(new ThisExpr, "statusCode"), new NameExpr("statusCode"), AssignExpr.Operator.ASSIGN))
          )
        )
      )

    cls
      .addMethod(s"getStatusCode", PUBLIC)
      .setType(PrimitiveType.intType)
      .setBody(
        new BlockStmt(
          new NodeList(
            new ReturnStmt(new FieldAccessExpr(new ThisExpr, "statusCode"))
          )
        )
      )

    Target.pure(cls)
  }

  def generateResponseClass(superClassType: ClassOrInterfaceType,
                            response: Response[JavaLanguage],
                            errorEntityFallbackType: Option[Type]): Target[(ClassOrInterfaceDeclaration, BodyDeclaration[_ <: BodyDeclaration[_]])] = {
    val clsName = response.statusCodeName.asString
    for {
      clsType <- safeParseClassOrInterfaceType(clsName)
    } yield {
      val cls = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC, STATIC), false, clsName)
        .setExtendedTypes(new NodeList(superClassType))

      val (classDecls, creator) = response.value
        .map(_._1)
        .orElse({
          if (response.statusCode >= 400 && response.statusCode <= 599) {
            errorEntityFallbackType
          } else {
            None
          }
        })
        .fold[(List[BodyDeclaration[_ <: BodyDeclaration[_]]], BodyDeclaration[_ <: BodyDeclaration[_]])]({
          val constructor = new ConstructorDeclaration(util.EnumSet.of(PRIVATE), clsName)
          constructor.setBody(
            new BlockStmt(
              new NodeList(
                new ExpressionStmt(
                  new MethodCallExpr(
                    "super",
                    new IntegerLiteralExpr(response.statusCode)
                  )
                )
              )
            )
          )

          val creator = new FieldDeclaration(
            util.EnumSet.of(PUBLIC, STATIC, FINAL),
            new VariableDeclarator(clsType, clsName, new ObjectCreationExpr(null, clsType, new NodeList))
          )

          (List(constructor), creator)
        })({ valueType =>
          val constructParam = new Parameter(util.EnumSet.of(FINAL), valueType.unbox, new SimpleName("entityBody"))

          val constructor = new ConstructorDeclaration(util.EnumSet.of(PRIVATE), clsName)
            .addParameter(constructParam)
            .setBody(
              new BlockStmt(
                new NodeList(
                  new ExpressionStmt(
                    new MethodCallExpr(
                      "super",
                      new IntegerLiteralExpr(response.statusCode)
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
            util.EnumSet.of(PRIVATE, FINAL),
            new VariableDeclarator(valueType, "entityBody")
          )

          val entityBodyGetter = new MethodDeclaration(util.EnumSet.of(PUBLIC), valueType, "getEntityBody")
            .setBody(
              new BlockStmt(
                new NodeList(
                  new ReturnStmt(new FieldAccessExpr(new ThisExpr, "entityBody"))
                )
              )
            )

          val creator = new MethodDeclaration(util.EnumSet.of(PUBLIC, STATIC), clsType, clsName)
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

  object ServerTermInterp extends (ServerTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ServerTerm[JavaLanguage, T]): Target[T] = term match {
      case GetExtraImports(tracing) =>
        List(
          "javax.inject.Inject",
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
          "java.util.Optional",
          "java.util.concurrent.CompletionStage",
          "org.glassfish.jersey.media.multipart.FormDataParam",
          "org.slf4j.Logger",
          "org.slf4j.LoggerFactory"
        ).traverse(safeParseRawImport)

      case BuildTracingFields(operation, resourceName, tracing) =>
        if (tracing) {
          Target.raiseError(s"Tracing is not yet supported by this framework")
        } else {
          Target.pure(Option.empty)
        }

      case GenerateRoutes(tracing, resourceName, basePath, routes, protocolElems, securitySchemes, authedRoutes) =>
        for {
          resourceType <- safeParseClassOrInterfaceType(resourceName)
          handlerName = s"${resourceName.replaceAll("Resource$", "")}Handler"
          handlerType <- safeParseClassOrInterfaceType(handlerName)
        } yield {
          val basePathComponents = basePath.toList.flatMap(splitPathComponents)
          val commonPathPrefix   = findPathPrefix(routes.map(_._3.path))

          val (routeMethods, handlerMethodSigs) = routes
            .map({
              case (operationId, tracingFields, sr @ RouteMeta(path, httpMethod, operation, securityRequirements), parameters, responses) =>
                parameters.parameters.foreach(p => p.param.setType(p.param.getType.unbox))

                val method = new MethodDeclaration(util.EnumSet.of(PUBLIC), new VoidType, operationId)
                  .addAnnotation(new MarkerAnnotationExpr(httpMethod.toString))

                val pathSuffix = splitPathComponents(path).drop(commonPathPrefix.length).mkString("/", "/", "")
                if (pathSuffix.nonEmpty && pathSuffix != "/") {
                  method.addAnnotation(new SingleMemberAnnotationExpr(new Name("Path"), new StringLiteralExpr(pathSuffix)))
                }

                val consumes = getBestConsumes(operation.consumes.flatMap(RouteMeta.ContentType.unapply).toList, parameters)
                  .orElse({
                    if (parameters.formParams.nonEmpty) {
                      if (parameters.formParams.exists(_.isFile)) {
                        Some(RouteMeta.MultipartFormData)
                      } else {
                        Some(RouteMeta.UrlencodedFormData)
                      }
                    } else if (parameters.bodyParams.nonEmpty) {
                      Some(RouteMeta.ApplicationJson)
                    } else {
                      None
                    }
                  })
                consumes
                  .map(c => new SingleMemberAnnotationExpr(new Name("Consumes"), new FieldAccessExpr(new NameExpr("MediaType"), c.toJaxRsAnnotationName)))
                  .foreach(method.addAnnotation)

                val successResponses =
                  operation.getResponses.entrySet.asScala.filter(entry => Try(entry.getKey.toInt / 100 == 2).getOrElse(false)).map(_.getValue).toList
                val produces = getBestProduces(operation.produces.flatMap(RouteMeta.ContentType.unapply).toList, successResponses, protocolElems)
                produces
                  .map(p => new SingleMemberAnnotationExpr(new Name("Produces"), new FieldAccessExpr(new NameExpr("MediaType"), p.toJaxRsAnnotationName)))
                  .foreach(method.addAnnotation)

                def addParamAnnotation(template: Parameter, annotationName: String, argName: String): Parameter = {
                  val parameter = template.clone()
                  parameter.addAnnotation(new SingleMemberAnnotationExpr(new Name(annotationName), new StringLiteralExpr(argName)))
                  parameter
                }

                val methodParams: List[Parameter] = List(
                  (parameters.pathParams, "PathParam"),
                  (parameters.headerParams, "HeaderParam"),
                  (parameters.queryStringParams, "QueryParam"),
                  (parameters.formParams, if (consumes.contains(RouteMeta.MultipartFormData)) "FormDataParam" else "FormParam")
                ).flatMap({
                  case (params, annotationName) =>
                    params.map(param => addParamAnnotation(param.param, annotationName, param.argName.value))
                }) ++ parameters.bodyParams.map(_.param)

                methodParams.foreach(method.addParameter)
                method.addParameter(
                  new Parameter(util.EnumSet.of(FINAL), ASYNC_RESPONSE_TYPE, new SimpleName("asyncResponse")).addMarkerAnnotation("Suspended")
                )

                val (responseName, responseType, resultResumeBody) =
                  ServerRawResponse(operation)
                    .filter(_ == true)
                    .fold({
                      val responseName = s"${handlerName}.${operationId.capitalize}Response"

                      val entitySetterIfTree = NonEmptyList
                        .fromList(responses.value.collect({
                          case Response(statusCodeName, Some(_)) => statusCodeName
                        }))
                        .map(_.reverse.foldLeft[IfStmt](null)({
                          case (nextIfTree, statusCodeName) =>
                            val responseSubclassType = JavaParser.parseClassOrInterfaceType(s"${responseName}.${statusCodeName}")
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
                        responseName,
                        JavaParser.parseClassOrInterfaceType(responseName),
                        (
                          List[Statement](
                            new ExpressionStmt(
                              new VariableDeclarationExpr(
                                new VariableDeclarator(
                                  RESPONSE_BUILDER_TYPE,
                                  "builder",
                                  new MethodCallExpr(new NameExpr("Response"),
                                                     "status",
                                                     new NodeList[Expression](new MethodCallExpr(new NameExpr("result"), "getStatusCode")))
                                ),
                                FINAL
                              )
                            )
                          ) ++ entitySetterIfTree ++ List(
                            new ExpressionStmt(
                              new MethodCallExpr(new NameExpr("asyncResponse"),
                                                 "resume",
                                                 new NodeList[Expression](new MethodCallExpr(new NameExpr("builder"), "build")))
                            )
                          )
                        ).toNodeList
                      )
                    })({ _ =>
                      (
                        "Response",
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

                val whenCompleteLambda = new LambdaExpr(
                  new NodeList(
                    new Parameter(util.EnumSet.of(FINAL), responseType, new SimpleName("result")),
                    new Parameter(util.EnumSet.of(FINAL), THROWABLE_TYPE, new SimpleName("err"))
                  ),
                  new BlockStmt(
                    new NodeList(
                      new IfStmt(
                        new BinaryExpr(new NameExpr("err"), new NullLiteralExpr, BinaryExpr.Operator.NOT_EQUALS),
                        new BlockStmt(
                          new NodeList(
                            new ExpressionStmt(
                              new MethodCallExpr(
                                new NameExpr("logger"),
                                "error",
                                new NodeList[Expression](
                                  new StringLiteralExpr(s"${handlerName}.${operationId} threw an exception ({}): {}"),
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
                                  new MethodCallExpr(new MethodCallExpr(
                                                       new NameExpr("Response"),
                                                       "status",
                                                       new NodeList[Expression](new IntegerLiteralExpr(500))
                                                     ),
                                                     "build")
                                )
                              )
                            )
                          )
                        ),
                        new BlockStmt(resultResumeBody)
                      )
                    )
                  ),
                  true
                )

                val handlerCall = new MethodCallExpr(
                  new FieldAccessExpr(new ThisExpr, "handler"),
                  operationId,
                  new NodeList[Expression](methodParams.map(param => new NameExpr(param.getName.asString)): _*)
                )

                method.setBody(
                  new BlockStmt(
                    new NodeList(
                      new ExpressionStmt(new MethodCallExpr(handlerCall, "whenComplete", new NodeList[Expression](whenCompleteLambda)))
                    )
                  )
                )

                val futureResponseType = completionStageType(responseType.clone())
                val handlerMethodSig   = new MethodDeclaration(util.EnumSet.noneOf(classOf[Modifier]), futureResponseType, operationId)
                (parameters.pathParams ++ parameters.headerParams ++ parameters.queryStringParams ++ parameters.formParams ++ parameters.bodyParams).foreach({
                  parameter =>
                    handlerMethodSig.addParameter(parameter.param.clone())
                })
                handlerMethodSig.setBody(null)

                (method, handlerMethodSig)
            })
            .unzip

          val resourceConstructor = new ConstructorDeclaration(util.EnumSet.of(PUBLIC), resourceName)
          resourceConstructor.addAnnotation(new MarkerAnnotationExpr(new Name("Inject")))
          resourceConstructor.addParameter(new Parameter(util.EnumSet.of(FINAL), handlerType, new SimpleName("handler")))
          resourceConstructor.setBody(
            new BlockStmt(
              new NodeList(
                new ExpressionStmt(new AssignExpr(new FieldAccessExpr(new ThisExpr, "handler"), new NameExpr("handler"), AssignExpr.Operator.ASSIGN))
              )
            )
          )

          val annotations = List(
            new SingleMemberAnnotationExpr(new Name("Path"), new StringLiteralExpr((basePathComponents ++ commonPathPrefix).mkString("/", "/", "")))
          )

          val supportDefinitions = List[BodyDeclaration[_ <: BodyDeclaration[_]]](
            new FieldDeclaration(
              util.EnumSet.of(PRIVATE, STATIC, FINAL),
              new VariableDeclarator(
                LOGGER_TYPE,
                "logger",
                new MethodCallExpr(new NameExpr("LoggerFactory"), "getLogger", new NodeList[Expression](new ClassExpr(resourceType)))
              )
            ),
            new FieldDeclaration(util.EnumSet.of(PRIVATE, FINAL), new VariableDeclarator(handlerType, "handler")),
            resourceConstructor
          )

          RenderedRoutes[JavaLanguage](routeMethods, List.empty, annotations, handlerMethodSigs, supportDefinitions, List.empty)
        }

      case GetExtraRouteParams(tracing) =>
        if (tracing) {
          Target.raiseError(s"Tracing is not yet supported by this framework")
        } else {
          Target.pure(List.empty)
        }

      case GenerateResponseDefinitions(operationId, responses, protocolElems) =>
        for {
          abstractResponseClassName <- safeParseSimpleName(s"${operationId.capitalize}Response").map(_.asString)
          abstractResponseClassType <- safeParseClassOrInterfaceType(abstractResponseClassName)

          // TODO: verify valueTypes are in protocolElems

          abstractResponseClass <- generateResponseSuperClass(abstractResponseClassName)
          responseClasses       <- responses.value.traverse(resp => generateResponseClass(abstractResponseClassType, resp, None))
        } yield {
          sortDefinitions(responseClasses.flatMap({ case (cls, creator) => List[BodyDeclaration[_ <: BodyDeclaration[_]]](cls, creator) }))
            .foreach(abstractResponseClass.addMember)

          abstractResponseClass :: Nil
        }

      case GenerateSupportDefinitions(tracing, securitySchemes) =>
        for {
          annotationImports <- List(
            "java.lang.annotation.ElementType",
            "java.lang.annotation.Retention",
            "java.lang.annotation.RetentionPolicy",
            "java.lang.annotation.Target",
            "javax.ws.rs.HttpMethod"
          ).traverse(safeParseRawImport)

          shower <- SerializationHelpers.showerSupportDef

          jersey <- SerializationHelpers.guardrailJerseySupportDef
        } yield {
          def httpMethodAnnotation(name: String): SupportDefinition[JavaLanguage] = {
            val annotationDecl = new AnnotationDeclaration(util.EnumSet.of(PUBLIC), name)
              .addAnnotation(
                new SingleMemberAnnotationExpr(new Name("Target"),
                                               new ArrayInitializerExpr(new NodeList(new FieldAccessExpr(new NameExpr("ElementType"), "METHOD"))))
              )
              .addAnnotation(new SingleMemberAnnotationExpr(new Name("Retention"), new FieldAccessExpr(new NameExpr("RetentionPolicy"), "RUNTIME")))
              .addAnnotation(new SingleMemberAnnotationExpr(new Name("HttpMethod"), new StringLiteralExpr(name)))
            SupportDefinition[JavaLanguage](new Name(name), annotationImports, annotationDecl)
          }

          List(
            shower,
            jersey,
            httpMethodAnnotation("PATCH"),
            httpMethodAnnotation("TRACE")
          )
        }

      case RenderClass(className, handlerName, classAnnotations, combinedRouteTerms, _, extraRouteParams, responseDefinitions, supportDefinitions) =>
        safeParseSimpleName(className) >>
          safeParseSimpleName(handlerName) >>
          Target.pure(doRenderClass(className, classAnnotations, supportDefinitions, combinedRouteTerms) :: Nil)

      case RenderHandler(handlerName, methodSigs, handlerDefinitions, responseDefinitions, securityRequirements) =>
        val handlerClass = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC), true, handlerName)
        sortDefinitions(methodSigs ++ responseDefinitions).foreach(handlerClass.addMember)
        Target.pure(handlerClass)
    }

    // Lift this function out of RenderClass above to work around a 2.11.x compiler syntax bug
    private def doRenderClass(className: String,
                              classAnnotations: List[AnnotationExpr],
                              supportDefinitions: List[BodyDeclaration[_ <: BodyDeclaration[_]]],
                              combinedRouteTerms: List[Node]): ClassOrInterfaceDeclaration = {
      val cls = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC), false, className)
      classAnnotations.foreach(cls.addAnnotation)
      sortDefinitions(supportDefinitions ++ combinedRouteTerms.collect({ case bd: BodyDeclaration[_] => bd }))
        .foreach(cls.addMember)
      cls
    }
  }
}
