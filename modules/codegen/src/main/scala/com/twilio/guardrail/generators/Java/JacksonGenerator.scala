package com.twilio.guardrail.generators.Java

import _root_.io.swagger.v3.oas.models.media.{ Discriminator => _, _ }
import cats.{ FlatMap, Monad }
import cats.data.NonEmptyList
import cats.syntax.all._
import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, PrimitiveType, Type, UnknownType }
import com.github.javaparser.ast.Modifier.Keyword.{ FINAL, PRIVATE, PROTECTED, PUBLIC }
import com.github.javaparser.ast.Modifier._
import com.github.javaparser.ast.{ Node, NodeList }
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.expr.{ MethodCallExpr, _ }
import com.github.javaparser.ast.stmt._
import com.twilio.guardrail.{
  DataRedacted,
  DataVisible,
  Discriminator,
  EmptyIsEmpty,
  ProtocolParameter,
  RedactionBehaviour,
  RuntimeFailure,
  StaticDefns,
  SuperClass,
  SwaggerUtil,
  Target,
  UserError
}
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.core.implicits._
import com.twilio.guardrail.extract.{ DataRedaction, EmptyValueIsNull }
import com.twilio.guardrail.generators.{ JavaGenerator, RawParameterName, RawParameterType }
import com.twilio.guardrail.generators.helpers.JacksonHelpers
import com.twilio.guardrail.generators.syntax.Java._
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.protocol.terms.protocol._
import com.twilio.guardrail.terms.CollectionsLibTerms
import com.twilio.guardrail.terms.collections.CollectionsAbstraction
import scala.collection.JavaConverters._
import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.{ Node, NodeList }
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.Modifier.Keyword.{ FINAL, PRIVATE, PROTECTED, PUBLIC }
import com.github.javaparser.ast.Modifier._
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.expr._
import java.math.BigInteger
import java.util.Locale

import com.twilio.guardrail.protocol.terms.protocol.PropertyConstraint.{ Maximum, Minimum }

import scala.util.Try

object JacksonGenerator {
  private val BUILDER_TYPE        = StaticJavaParser.parseClassOrInterfaceType("Builder")
  private val BIG_INTEGER_FQ_TYPE = StaticJavaParser.parseClassOrInterfaceType("java.math.BigInteger")
  private val BIG_DECIMAL_FQ_TYPE = StaticJavaParser.parseClassOrInterfaceType("java.math.BigDecimal")

  private case class ParameterTerm(
      propertyName: String,
      parameterName: String,
      fieldType: Type,
      parameterType: Type,
      rawType: RawParameterType,
      defaultValue: Option[Expression],
      dataRedacted: RedactionBehaviour,
      constraints: Set[PropertyConstraint]
  )

  // returns a tuple of (requiredTerms, optionalTerms)
  // note that required terms _that have a default value_ are conceptually optional.
  private def sortParams(
      params: List[ProtocolParameter[JavaLanguage]]
  )(implicit Ca: CollectionsAbstraction[JavaLanguage]): (List[ParameterTerm], List[ParameterTerm]) = {
    import Ca._

    def defaultValueToExpression(defaultValue: Option[Node]): Option[Expression] = defaultValue match {
      case Some(expr: Expression) => Some(expr)
      case _                      => None
    }

    params
      .map({ param =>
        val parameterType = if (param.term.getType.isOptionalType) {
          param.term.getType.containedType.unbox
        } else {
          param.term.getType.unbox
        }
        val defaultValue = defaultValueToExpression(param.defaultValue)

        ParameterTerm(
          param.name.value,
          param.term.getNameAsString,
          param.term.getType.unbox,
          parameterType,
          param.rawType,
          defaultValue,
          param.dataRedaction,
          param.constraints
        )
      })
      .partition(
        pt => !pt.fieldType.isOptionalType && pt.defaultValue.isEmpty
      )
  }

  private def addParents(cls: ClassOrInterfaceDeclaration, parentOpt: Option[SuperClass[JavaLanguage]]): Unit =
    parentOpt.foreach({ parent =>
      val directParent = StaticJavaParser.parseClassOrInterfaceType(parent.clsName)
      val otherParents = parent.interfaces.map(StaticJavaParser.parseClassOrInterfaceType)
      val _ = cls
        .setExtendedTypes(new NodeList(directParent))
        .setImplementedTypes(otherParents.toNodeList)
    })

  private def lookupTypeName(tpeName: String, concreteTypes: List[PropMeta[JavaLanguage]]): Option[Type] =
    concreteTypes
      .find(_.clsName == tpeName)
      .map(_.tpe)

  // TODO: handle emptyToNull in the return for the getters
  private def addParameterGetter(cls: ClassOrInterfaceDeclaration, param: ParameterTerm): Unit = {
    val _ = cls
      .addMethod(getterMethodNameForParameter(param.parameterName), PUBLIC)
      .setType(param.fieldType)
      .setBody(
        new BlockStmt(
          new NodeList(
            new ReturnStmt(new FieldAccessExpr(new ThisExpr, param.parameterName))
          )
        )
      )
  }

  private def dtoConstructorBody(
      superCall: Expression,
      terms: List[ParameterTerm]
  )(implicit Cl: CollectionsLibTerms[JavaLanguage, Target], Ca: CollectionsAbstraction[JavaLanguage], fm: FlatMap[Target]): Target[BlockStmt] = {
    import Ca._
    for {
      emptyOptional <- Cl.emptyOptionalTerm().flatMap(_.toExpression)
    } yield new BlockStmt(
      (
        List[Statement](new ExpressionStmt(superCall)) ++
            terms
              .map(
                term =>
                  new ExpressionStmt(
                    new AssignExpr(
                      new FieldAccessExpr(new ThisExpr, term.parameterName),
                      term.fieldType match {
                        case _: PrimitiveType => new NameExpr(term.parameterName)
                        case ft if ft.isOptionalType =>
                          new ConditionalExpr(
                            new BinaryExpr(new NameExpr(term.parameterName), new NullLiteralExpr, BinaryExpr.Operator.EQUALS),
                            emptyOptional,
                            new NameExpr(term.parameterName)
                          )
                        case _ => requireNonNullExpr(term.parameterName)
                      },
                      AssignExpr.Operator.ASSIGN
                    )
                  )
              )
      ).toNodeList
    )
  }

  def EnumProtocolTermInterp(implicit Cl: CollectionsLibTerms[JavaLanguage, Target]): EnumProtocolTerms[JavaLanguage, Target] = new EnumProtocolTermInterp
  class EnumProtocolTermInterp(implicit Cl: CollectionsLibTerms[JavaLanguage, Target]) extends EnumProtocolTerms[JavaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances
    def extractEnum(swagger: Tracker[Schema[_]]) = {
      val enumEntries: List[String] = swagger
        .refine({ case x: StringSchema => x })(_.downField("enum", _.getEnum()))
        .orRefineFallback(_.downField("enum", _.getEnum()).map(_.toList.flatMap(_.asScala.toList).map(_.toString())))
        .unwrapTracker
      Target.pure(Option(enumEntries).filterNot(_.isEmpty).toRight("Model has no enumerations"))
    }

    def renderMembers(
        clsName: String,
        elems: List[(String, com.github.javaparser.ast.expr.Name, com.github.javaparser.ast.expr.Name)]
    ) =
      Target.pure(None)

    def encodeEnum(clsName: String): Target[Option[BodyDeclaration[_ <: BodyDeclaration[_]]]] =
      Target.pure(None)

    def decodeEnum(clsName: String): Target[Option[BodyDeclaration[_ <: BodyDeclaration[_]]]] =
      Target.pure(None)

    def renderClass(
        clsName: String,
        tpe: com.github.javaparser.ast.`type`.Type,
        elems: List[(String, com.github.javaparser.ast.expr.Name, com.github.javaparser.ast.expr.Name)]
    ) = {
      val enumType = StaticJavaParser.parseType(clsName)

      val enumDefns = elems.map {
        case (value, termName, _) =>
          new EnumConstantDeclaration(
            new NodeList(),
            new SimpleName(termName.getIdentifier),
            new NodeList(new StringLiteralExpr(value)),
            new NodeList()
          )
      }

      val nameField = new FieldDeclaration(
        new NodeList(privateModifier, finalModifier),
        new VariableDeclarator(STRING_TYPE, "name")
      )

      val constructor = new ConstructorDeclaration(new NodeList(privateModifier), clsName)
        .addParameter(new Parameter(new NodeList(finalModifier), STRING_TYPE, new SimpleName("name")))
        .setBody(
          new BlockStmt(
            new NodeList(
              new ExpressionStmt(
                new AssignExpr(
                  new FieldAccessExpr(new ThisExpr, "name"),
                  new NameExpr("name"),
                  AssignExpr.Operator.ASSIGN
                )
              )
            )
          )
        )

      val getNameMethod = new MethodDeclaration(
        new NodeList(publicModifier),
        STRING_TYPE,
        "getName"
      ).addMarkerAnnotation("JsonValue")
        .setBody(
          new BlockStmt(
            new NodeList(
              new ReturnStmt(new FieldAccessExpr(new ThisExpr, "name"))
            )
          )
        )

      val fromStringMethod = new MethodDeclaration(
        new NodeList(publicModifier, staticModifier),
        enumType,
        "fromString"
      ).addMarkerAnnotation("JsonCreator")
        .addParameter(new Parameter(new NodeList(finalModifier), STRING_TYPE, new SimpleName("name")))
        .setBody(
          new BlockStmt(
            new NodeList(
              new ForEachStmt(
                new VariableDeclarationExpr(new VariableDeclarator(enumType, "value"), finalModifier),
                new MethodCallExpr("values"),
                new BlockStmt(
                  new NodeList(
                    new IfStmt(
                      new MethodCallExpr("value.name.equals", new NameExpr("name")),
                      new ReturnStmt(new NameExpr("value")),
                      null
                    )
                  )
                )
              ),
              new ThrowStmt(
                new ObjectCreationExpr(
                  null,
                  StaticJavaParser.parseClassOrInterfaceType("IllegalArgumentException"),
                  new NodeList(
                    new BinaryExpr(
                      new BinaryExpr(new StringLiteralExpr("Name '"), new NameExpr("name"), BinaryExpr.Operator.PLUS),
                      new StringLiteralExpr(s"' is not valid for enum '${clsName}'"),
                      BinaryExpr.Operator.PLUS
                    )
                  )
                )
              )
            )
          )
        )

      val compatParseMethod = new MethodDeclaration(
        new NodeList(publicModifier, staticModifier),
        enumType,
        "parse"
      ).addMarkerAnnotation("Deprecated")
        .setJavadocComment("@deprecated See {@link #fromString(String)}")
        .addParameter(new Parameter(new NodeList(finalModifier), STRING_TYPE, new SimpleName("name")))
        .setBody(
          new BlockStmt(
            new NodeList(
              new ReturnStmt(new MethodCallExpr("fromString", new NameExpr("name")))
            )
          )
        )

      val staticInitializer = new InitializerDeclaration(
        true,
        new BlockStmt(
          new NodeList(
            new ExpressionStmt(
              new MethodCallExpr(
                new MethodCallExpr(new NameExpr("Shower"), "getInstance"),
                "register",
                new NodeList[Expression](
                  new ClassExpr(StaticJavaParser.parseClassOrInterfaceType(clsName)),
                  new MethodReferenceExpr(new NameExpr(clsName), null, "getName")
                )
              )
            )
          )
        )
      )

      val enumClass = new EnumDeclaration(
        new NodeList(publicModifier),
        new NodeList(),
        new SimpleName(clsName),
        new NodeList(),
        new NodeList(enumDefns: _*),
        new NodeList(
          staticInitializer,
          nameField,
          constructor,
          getNameMethod,
          fromStringMethod,
          compatParseMethod
        )
      )

      Target.pure(enumClass)
    }

    def renderStaticDefns(
        clsName: String,
        members: Option[Nothing],
        accessors: List[com.github.javaparser.ast.expr.Name],
        encoder: Option[com.github.javaparser.ast.body.BodyDeclaration[_ <: BodyDeclaration[_]]],
        decoder: Option[com.github.javaparser.ast.body.BodyDeclaration[_ <: BodyDeclaration[_]]]
    ): Target[StaticDefns[JavaLanguage]] =
      for {
        extraImports <- List(
          "com.fasterxml.jackson.annotation.JsonCreator",
          "com.fasterxml.jackson.annotation.JsonValue"
        ).traverse(safeParseRawImport)
      } yield StaticDefns[JavaLanguage](
        className = clsName,
        extraImports = extraImports,
        definitions = List.empty
      )

    def buildAccessor(clsName: String, termName: String) =
      Target.pure(new Name(s"${clsName}.${termName}"))
  }

  def ModelProtocolTermInterp(
      implicit Cl: CollectionsLibTerms[JavaLanguage, Target],
      Ca: CollectionsAbstraction[JavaLanguage]
  ): ModelProtocolTerms[JavaLanguage, Target] =
    new ModelProtocolTermInterp

  class ModelProtocolTermInterp(implicit Cl: CollectionsLibTerms[JavaLanguage, Target], Ca: CollectionsAbstraction[JavaLanguage])
      extends ModelProtocolTerms[JavaLanguage, Target] {
    import Ca._

    implicit def MonadF: Monad[Target] = Target.targetInstances

    def renderDTOClass(
        clsName: String,
        supportPackage: List[String],
        selfParams: List[ProtocolParameter[JavaLanguage]],
        parents: List[SuperClass[JavaLanguage]]
    ): Target[TypeDeclaration[_ <: TypeDeclaration[_]]] = {
      val parentsWithDiscriminators = parents.collect({ case p if p.discriminators.nonEmpty => p })
      val discriminators            = parents.flatMap(_.discriminators)
      val discriminatorNames        = discriminators.map(_.propertyName).toSet

      def withoutDiscriminators(terms: List[ParameterTerm]): List[ParameterTerm] =
        terms.filterNot(term => discriminatorNames.contains(term.propertyName))

      def parameterGetterCall(term: ParameterTerm, scope: Option[String] = None): MethodCallExpr = {
        val methodName = getterMethodNameForParameter(term.parameterName)
        scope.fold(new MethodCallExpr(methodName))(s => new MethodCallExpr(new NameExpr(s), methodName))
      }

      def parameterToStringExpr(term: ParameterTerm, scope: Option[String] = None): Expression = term.dataRedacted match {
        case DataVisible  => parameterGetterCall(term, scope)
        case DataRedacted => new StringLiteralExpr("[redacted]")
      }

      for {
        dtoClassType <- safeParseClassOrInterfaceType(clsName)
        parentOpt <- (parentsWithDiscriminators, parents) match {
          case _ if parentsWithDiscriminators.length > 1 =>
            Target.raiseUserError[Option[SuperClass[JavaLanguage]]](
              s"${clsName} requires unsupported multiple inheritance due to multiple parents with discriminators (${parentsWithDiscriminators.map(_.clsName).mkString(", ")})"
            )
          case _ if parentsWithDiscriminators.length == 1 => Target.pure(parentsWithDiscriminators.headOption)
          case _ if parents.length == 1                   => Target.pure(parents.headOption)
          case _                                          => Target.pure(None)
        }

        parentParams                               = parentOpt.toList.flatMap(_.params)
        parentParamNames                           = parentParams.map(_.name.value)
        (parentRequiredTerms, parentOptionalTerms) = sortParams(parentParams)
        parentTerms                                = parentRequiredTerms ++ parentOptionalTerms

        discriminatorValues <- parentTerms
          .flatMap({ term =>
            discriminators.find(_.propertyName == term.propertyName).map((term, _))
          })
          .traverse({
            case (term, discriminator) =>
              val discriminatorValue = discriminator.mapping
                .collectFirst({ case (value, elem) if elem.name == clsName => value })
                .getOrElse(clsName)

              JacksonHelpers
                .discriminatorExpression[JavaLanguage](
                  discriminator.propertyName,
                  discriminatorValue,
                  term.rawType.tpe,
                  term.rawType.format
                )(
                  v => Target.pure[Node](new ObjectCreationExpr(null, BIG_INTEGER_FQ_TYPE, new NodeList(new StringLiteralExpr(v)))),
                  v => Target.pure[Node](new ObjectCreationExpr(null, BIG_DECIMAL_FQ_TYPE, new NodeList(new StringLiteralExpr(v)))),
                  v =>
                    term.fieldType match {
                      case cls: ClassOrInterfaceType =>
                        // hopefully it's an enum type; nothing else really makes sense here
                        JavaGenerator.JavaInterp.formatEnumName(v).map(ev => new FieldAccessExpr(cls.getNameAsExpression, ev))
                      case tpe =>
                        Target.raiseUserError[Node](s"Unsupported discriminator type '${tpe.asString}' for property '${term.propertyName}'")
                    }
                )(JavaGenerator.JavaInterp)
                .flatMap[Expression]({
                  case expr: Expression => Target.pure(expr)
                  case node =>
                    Target.raiseError(
                      RuntimeFailure(s"BUG: JacksonHelpers.discriminatorExpression() returned a ${node.getClass.getSimpleName} when we need an Expression")
                    )
                })
                .map((term.propertyName, _))
          })
          .map(_.toMap)

        params = parents.filterNot(parent => parentOpt.contains(parent)).flatMap(_.params) ++ selfParams.filterNot(
              param => discriminatorNames.contains(param.term.getName.getIdentifier) || parentParamNames.contains(param.term.getName.getIdentifier)
            )
        (requiredTerms, optionalTerms) = sortParams(params)
        terms                          = requiredTerms ++ optionalTerms

        dtoClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier), false, clsName)
          .addAnnotation(
            new NormalAnnotationExpr(
              new Name("JsonIgnoreProperties"),
              new NodeList(
                new MemberValuePair(
                  "ignoreUnknown",
                  new BooleanLiteralExpr(true)
                )
              )
            )
          )
          .addAnnotation(
            generatedAnnotation(JacksonGenerator.getClass)
          )

        _ = addParents(dtoClass, parentOpt)

        _ = terms.foreach({
          case ParameterTerm(propertyName, parameterName, fieldType, _, _, _, _, constraints) =>
            val field: FieldDeclaration = dtoClass.addField(fieldType, parameterName, PRIVATE, FINAL)
            field.addSingleMemberAnnotation("JsonProperty", new StringLiteralExpr(propertyName))
            for (constraint <- constraints) yield (constraint) match {
              case Minimum(value) => field.addSingleMemberAnnotation("Min", new IntegerLiteralExpr(value.toString))
              case Maximum(value) => field.addSingleMemberAnnotation("Max", new IntegerLiteralExpr(value.toString))
            }
        })

        primaryConstructor = dtoClass
          .addConstructor(PROTECTED)
          .addMarkerAnnotation("JsonCreator")
          .setParameters(
            new NodeList(
              withoutDiscriminators(parentTerms ++ terms).map({
                case ParameterTerm(propertyName, parameterName, fieldType, _, _, _, _, _) =>
                  new Parameter(new NodeList(finalModifier), fieldType, new SimpleName(parameterName))
                    .addAnnotation(new SingleMemberAnnotationExpr(new Name("JsonProperty"), new StringLiteralExpr(propertyName)))
              }): _*
            )
          )
        superCall = new MethodCallExpr(
          "super",
          parentTerms.map(term => discriminatorValues.getOrElse(term.propertyName, new NameExpr(term.parameterName))): _*
        )
        primaryConstructorBody <- dtoConstructorBody(superCall, terms)
        _ = primaryConstructor.setBody(primaryConstructorBody)

        _ = terms.foreach(addParameterGetter(dtoClass, _))

        toStringFieldExprs = NonEmptyList
          .fromList(parentTerms ++ terms)
          .toList
          .flatMap(
            l =>
              (new StringLiteralExpr(s"${l.head.parameterName}="), parameterToStringExpr(l.head)) +:
                  l.tail.map(
                    term =>
                      (
                        new StringLiteralExpr(s", ${term.parameterName}="),
                        parameterToStringExpr(term)
                      )
                  )
          )

        _ = dtoClass
          .addMethod("toString", PUBLIC)
          .setType(STRING_TYPE)
          .addMarkerAnnotation("Override")
          .setBody(
            new BlockStmt(
              new NodeList(
                new ReturnStmt(
                  new BinaryExpr(
                    toStringFieldExprs.foldLeft[Expression](new StringLiteralExpr(s"${clsName}{"))({
                      case (prevExpr, (strExpr, fieldExpr)) =>
                        new BinaryExpr(
                          new BinaryExpr(prevExpr, strExpr, BinaryExpr.Operator.PLUS),
                          fieldExpr,
                          BinaryExpr.Operator.PLUS
                        )
                    }),
                    new StringLiteralExpr("}"),
                    BinaryExpr.Operator.PLUS
                  )
                )
              )
            )
          )

        equalsConditions: List[Expression] = terms.map(
          term =>
            term.fieldType match {
              case _: PrimitiveType =>
                new BinaryExpr(
                  parameterGetterCall(term),
                  parameterGetterCall(term, Some("other")),
                  BinaryExpr.Operator.EQUALS
                )
              case _ =>
                new MethodCallExpr(
                  new NameExpr("java.util.Objects"),
                  "equals",
                  new NodeList[Expression](new FieldAccessExpr(new ThisExpr, term.parameterName), parameterGetterCall(term, Some("other")))
                )
            }
        )
        returnExpr = NonEmptyList
          .fromList(equalsConditions)
          .map(
            _.reduceLeft(
              (prevExpr, condExpr) => new BinaryExpr(prevExpr, condExpr, BinaryExpr.Operator.AND)
            )
          )
          .getOrElse(new BooleanLiteralExpr(true))

        _ = dtoClass
          .addMethod("equals", PUBLIC)
          .setType(PrimitiveType.booleanType)
          .addMarkerAnnotation("Override")
          .addParameter(new Parameter(new NodeList(finalModifier), OBJECT_TYPE, new SimpleName("o")))
          .setBody(
            new BlockStmt(
              new NodeList(
                new IfStmt(
                  new BinaryExpr(new ThisExpr, new NameExpr("o"), BinaryExpr.Operator.EQUALS),
                  new BlockStmt(new NodeList(new ReturnStmt(new BooleanLiteralExpr(true)))),
                  null
                ),
                new IfStmt(
                  new BinaryExpr(
                    new BinaryExpr(new NameExpr("o"), new NullLiteralExpr, BinaryExpr.Operator.EQUALS),
                    new BinaryExpr(new MethodCallExpr("getClass"), new MethodCallExpr(new NameExpr("o"), "getClass"), BinaryExpr.Operator.NOT_EQUALS),
                    BinaryExpr.Operator.OR
                  ),
                  new BlockStmt(new NodeList(new ReturnStmt(new BooleanLiteralExpr(false)))),
                  null
                ),
                new ExpressionStmt(
                  new VariableDeclarationExpr(
                    new VariableDeclarator(
                      dtoClassType,
                      "other",
                      new CastExpr(dtoClassType, new NameExpr("o"))
                    ),
                    finalModifier
                  )
                ),
                new ReturnStmt(returnExpr)
              )
            )
          )

        _ = dtoClass
          .addMethod("hashCode", PUBLIC)
          .setType(PrimitiveType.intType)
          .addMarkerAnnotation("Override")
          .setBody(
            new BlockStmt(
              new NodeList(
                new ReturnStmt(
                  new MethodCallExpr(
                    new NameExpr("java.util.Objects"),
                    "hash",
                    new NodeList[Expression]((parentTerms ++ terms).map(parameterGetterCall(_, None)): _*)
                  )
                )
              )
            )
          )

        builderClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier, staticModifier), false, "Builder")

        _ = withoutDiscriminators(parentRequiredTerms ++ requiredTerms).foreach({
          case ParameterTerm(_, parameterName, fieldType, _, _, _, _, _) =>
            builderClass.addField(fieldType, parameterName, PRIVATE)
        })
        _ <- withoutDiscriminators(parentOptionalTerms ++ optionalTerms).traverse({
          case ParameterTerm(_, parameterName, fieldType, _, _, defaultValue, _, _) =>
            for {
              initializer <- defaultValue.fold[Target[Expression]](
                Cl.emptyOptionalTerm().flatMap(_.toExpression)
              )(
                dv =>
                  if (fieldType.isOptionalType) {
                    Cl.liftOptionalTerm(dv).flatMap(_.toExpression)
                  } else {
                    Target.pure(dv)
                  }
              )
              _ = builderClass.addFieldWithInitializer(fieldType, parameterName, initializer, PRIVATE)
            } yield ()
        })

        _ = builderClass
          .addConstructor(PUBLIC)
          .setParameters(
            new NodeList(
              withoutDiscriminators(parentRequiredTerms ++ requiredTerms).map({
                case ParameterTerm(_, parameterName, _, parameterType, _, _, _, _) =>
                  new Parameter(new NodeList(finalModifier), parameterType, new SimpleName(parameterName))
              }): _*
            )
          )
          .setBody(
            new BlockStmt(
              new NodeList(
                withoutDiscriminators(parentRequiredTerms ++ requiredTerms).map({
                  case ParameterTerm(_, parameterName, fieldType, _, _, _, _, _) =>
                    new ExpressionStmt(
                      new AssignExpr(
                        new FieldAccessExpr(new ThisExpr, parameterName),
                        fieldType match {
                          case _: PrimitiveType => new NameExpr(parameterName)
                          case _                => requireNonNullExpr(parameterName)
                        },
                        AssignExpr.Operator.ASSIGN
                      )
                    )
                }): _*
              )
            )
          )

        _ = builderClass
          .addConstructor(PUBLIC)
          .setParameters(new NodeList(new Parameter(new NodeList(finalModifier), dtoClassType, new SimpleName("template"))))
          .setBody(
            new BlockStmt(
              withoutDiscriminators(parentTerms ++ terms)
                .map({
                  case term @ ParameterTerm(_, parameterName, _, _, _, _, _, _) =>
                    new ExpressionStmt(
                      new AssignExpr(
                        new FieldAccessExpr(new ThisExpr, parameterName),
                        parameterGetterCall(term, Some("template")),
                        AssignExpr.Operator.ASSIGN
                      )
                    ): Statement
                })
                .toNodeList
            )
          )

        // TODO: leave out with${name}() if readOnlyKey?
        _ <- withoutDiscriminators(parentTerms ++ terms).traverse({
          case ParameterTerm(_, parameterName, fieldType, parameterType, _, _, _, _) =>
            val methodName = s"with${parameterName.unescapeIdentifier.capitalize}"
            for {
              fieldInitializer <- (fieldType, parameterType) match {
                case (_: PrimitiveType, _) =>
                  Target.pure[Expression](new NameExpr(parameterName))
                case (ft, pt) if ft.isOptionalType && pt.isPrimitiveType =>
                  Cl.liftSomeTerm(new NameExpr(parameterName)).flatMap(_.toExpression)
                case (ft, _) if ft.isOptionalType =>
                  Cl.liftOptionalTerm(new NameExpr(parameterName)).flatMap(_.toExpression)
                case _ =>
                  Target.pure(requireNonNullExpr(parameterName))
              }

              _ = builderClass
                .addMethod(methodName, PUBLIC)
                .setType(BUILDER_TYPE)
                .addParameter(new Parameter(new NodeList(finalModifier), parameterType, new SimpleName(parameterName)))
                .setBody(
                  new BlockStmt(
                    new NodeList(
                      new ExpressionStmt(
                        new AssignExpr(
                          new FieldAccessExpr(new ThisExpr, parameterName),
                          fieldInitializer,
                          AssignExpr.Operator.ASSIGN
                        )
                      ),
                      new ReturnStmt(new ThisExpr)
                    )
                  )
                )

              _ = if (!parameterType.isOptionalType) {
                val newParameterName = s"optional${parameterName.unescapeIdentifier.capitalize}"
                for {
                  newParameterType <- (fieldType match {
                    case pt: PrimitiveType       => Cl.liftOptionalType(pt.toBoxedType)
                    case ft if ft.isOptionalType => Target.pure(ft)
                    case ft                      => Cl.liftOptionalType(ft)
                  })
                } yield {
                  builderClass
                    .addMethod(methodName, PUBLIC)
                    .setType(BUILDER_TYPE)
                    .addParameter(new Parameter(new NodeList(finalModifier), newParameterType, new SimpleName(newParameterName)))
                    .setBody(
                      new BlockStmt(
                        new NodeList(
                          new ExpressionStmt(
                            if (fieldType.isOptionalType) {
                              new AssignExpr(
                                new FieldAccessExpr(new ThisExpr, parameterName),
                                requireNonNullExpr(newParameterName),
                                AssignExpr.Operator.ASSIGN
                              )
                            } else {
                              requireNonNullExpr(newParameterName)
                                .lift[Option[Any]]
                                .foreach(
                                  new LambdaExpr(
                                    new Parameter(new UnknownType, parameterName),
                                    new AssignExpr(
                                      new FieldAccessExpr(new ThisExpr, parameterName),
                                      new NameExpr(parameterName),
                                      AssignExpr.Operator.ASSIGN
                                    )
                                  ).lift[Any => Unit]
                                )
                                .value
                            }
                          ),
                          new ReturnStmt(new ThisExpr)
                        )
                      )
                    )
                }
              } else {
                ()
              }
            } yield ()
        })

        builderBuildTerms = withoutDiscriminators(parentTerms ++ terms)
        _ = builderClass
          .addMethod("build", PUBLIC)
          .setType(clsName)
          .setBody(
            new BlockStmt(
              (
                builderBuildTerms
                  .filterNot(_.fieldType.isPrimitiveType)
                  .map(term => new ExpressionStmt(requireNonNullExpr(new FieldAccessExpr(new ThisExpr, term.parameterName)))) :+ new ReturnStmt(
                      new ObjectCreationExpr(
                        null,
                        StaticJavaParser.parseClassOrInterfaceType(clsName),
                        new NodeList(
                          builderBuildTerms.map(param => new FieldAccessExpr(new ThisExpr, param.parameterName)): _*
                        )
                      )
                    )
              ).toNodeList
            )
          )

        _ = dtoClass.addMember(builderClass)

      } yield dtoClass
    }

    def extractProperties(swagger: Tracker[Schema[_]]) =
      swagger
        .refine({ case m: ObjectSchema => m })(m => Target.pure(m.downField("properties", _.getProperties()).indexedCosequence.value))
        .orRefine({ case c: ComposedSchema => c })(
          comp =>
            Target.pure(
              comp
                .downField("allOf", _.getAllOf())
                .indexedDistribute
                .lastOption
                .toList
                .flatMap(_.downField("properties", _.getProperties).indexedCosequence.value.toList)
            )
        )
        .orRefine({ case x: Schema[_] if Option(x.get$ref()).isDefined => x })(
          comp => Target.raiseUserError(s"Attempted to extractProperties for a ${comp.unwrapTracker.getClass()}, unsure what to do here (${comp.showHistory})")
        )
        .getOrElse(Target.pure(List.empty[(String, Tracker[Schema[_]])]))

    def transformProperty(
        clsName: String,
        dtoPackage: List[String],
        supportPackage: List[String],
        concreteTypes: List[PropMeta[JavaLanguage]]
    )(
        name: String,
        fieldName: String,
        property: Schema[_],
        meta: SwaggerUtil.ResolvedType[JavaLanguage],
        requirement: PropertyRequirement,
        constraints: Set[PropertyConstraint],
        isCustomType: Boolean,
        defaultValue: Option[com.github.javaparser.ast.Node]
    ) =
      Target.log.function("transformProperty") {
        val readOnlyKey = Option(name).filter(_ => Option(property.getReadOnly).contains(true))
        val emptyToNull = (property match {
          case d: DateSchema      => EmptyValueIsNull(d)
          case dt: DateTimeSchema => EmptyValueIsNull(dt)
          case s: StringSchema    => EmptyValueIsNull(s)
          case _                  => None
        }).getOrElse(EmptyIsEmpty)
        val dataRedaction = DataRedaction(property).getOrElse(DataVisible)
        for {
          tpeClassDep <- meta match {
            case SwaggerUtil.Resolved(declType, classDep, _, _, _) =>
              Target.pure((declType, classDep))
            case SwaggerUtil.Deferred(tpeName) =>
              val tpe = concreteTypes.find(_.clsName == tpeName).map(x => Target.pure(x.tpe)).getOrElse {
                println(s"Unable to find definition for ${tpeName}, just inlining")
                safeParseType(tpeName)
              }
              tpe.map((_, Option.empty))
            case SwaggerUtil.DeferredArray(tpeName, containerTpe) =>
              val concreteType = lookupTypeName(tpeName, concreteTypes)
              for {
                innerType <- concreteType.fold(safeParseType(tpeName))(Target.pure)
                tpe       <- Cl.liftVectorType(innerType, containerTpe)
              } yield (tpe, Option.empty)
            case SwaggerUtil.DeferredMap(tpeName, containerTpe) =>
              val concreteType = lookupTypeName(tpeName, concreteTypes)
              for {
                innerType <- concreteType.fold(safeParseType(tpeName))(Target.pure)
                tpe       <- Cl.liftMapType(innerType, containerTpe)
              } yield (tpe, Option.empty)
          }
          (tpe, classDep) = tpeClassDep

          rawType = RawParameterType(Option(property.getType), Option(property.getFormat))

          expressionDefaultValue <- (defaultValue match {
            case Some(e: Expression) => Target.pure(Some(e))
            case Some(_) =>
              Target.log.warning(s"Can't generate default value for class $clsName and property $name.") >> Target.pure(None)
            case None => Target.pure(None)
          }): Target[Option[Expression]]
          finalDefaultTypeValue <- Option(requirement)
            .filter {
              case PropertyRequirement.Required => true
              case _                            => false
            }
            .fold[Target[(Type, Option[Expression])]](
              for {
                optionalTpe      <- Cl.liftOptionalType(tpe)
                defaultValueExpr <- defaultValue.fold(Target.pure(Option.empty[Expression]))(dv => dv.toExpression.map(Option.apply))
              } yield (optionalTpe, defaultValueExpr)
            )(Function.const(Target.pure((tpe, expressionDefaultValue))) _)
          (finalDeclType, finalDefaultValue) = finalDefaultTypeValue
          term <- safeParseParameter(s"final ${finalDeclType} $fieldName")
          dep = classDep.filterNot(_.asString == clsName) // Filter out our own class name
        } yield ProtocolParameter[JavaLanguage](
          term,
          finalDeclType,
          RawParameterName(name),
          dep,
          rawType,
          readOnlyKey,
          emptyToNull,
          dataRedaction,
          requirement,
          finalDefaultValue,
          constraints
        )
      }

    def encodeModel(
        clsName: String,
        dtoPackage: List[String],
        selfParams: List[ProtocolParameter[JavaLanguage]],
        parents: List[SuperClass[JavaLanguage]] = Nil
    ) =
      Target.pure(None)

    def decodeModel(
        clsName: String,
        dtoPackage: List[String],
        supportPackage: List[String],
        selfParams: List[ProtocolParameter[JavaLanguage]],
        parents: List[SuperClass[JavaLanguage]] = Nil
    ) =
      Target.pure(None)

    def renderDTOStaticDefns(
        clsName: String,
        deps: List[com.github.javaparser.ast.expr.Name],
        encoder: Option[com.github.javaparser.ast.body.VariableDeclarator],
        decoder: Option[com.github.javaparser.ast.body.VariableDeclarator]
    ) =
      Target.pure(StaticDefns(clsName, List.empty, List.empty))
  }

  def ArrayProtocolTermInterp(implicit Cl: CollectionsLibTerms[JavaLanguage, Target]): ArrayProtocolTerms[JavaLanguage, Target] = new ArrayProtocolTermInterp
  class ArrayProtocolTermInterp(implicit Cl: CollectionsLibTerms[JavaLanguage, Target]) extends ArrayProtocolTerms[JavaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances
    def extractArrayType(
        arr: SwaggerUtil.ResolvedType[JavaLanguage],
        concreteTypes: List[PropMeta[JavaLanguage]]
    ): Target[Type] =
      for {
        result <- arr match {
          case SwaggerUtil.Resolved(tpe, dep, default, _, _) => Target.pure(tpe)
          case SwaggerUtil.Deferred(tpeName) =>
            Target.fromOption(lookupTypeName(tpeName, concreteTypes), UserError(s"Unresolved reference ${tpeName}"))
          case SwaggerUtil.DeferredArray(tpeName, containerTpe) =>
            lookupTypeName(tpeName, concreteTypes)
              .fold[Target[Type]](Target.raiseUserError(s"Unresolved reference ${tpeName}"))(Cl.liftVectorType(_, containerTpe))
          case SwaggerUtil.DeferredMap(tpeName, containerTpe) =>
            lookupTypeName(tpeName, concreteTypes).fold[Target[Type]](
              Target.raiseUserError(s"Unresolved reference ${tpeName}")
            )(tpe => Cl.liftMapType(tpe, None).flatMap(mapTpe => Cl.liftVectorType(mapTpe, containerTpe)))
        }
      } yield result
  }

  def ProtocolSupportTermInterp(implicit Cl: CollectionsLibTerms[JavaLanguage, Target]): ProtocolSupportTerms[JavaLanguage, Target] =
    new ProtocolSupportTermInterp
  class ProtocolSupportTermInterp(implicit Cl: CollectionsLibTerms[JavaLanguage, Target]) extends ProtocolSupportTerms[JavaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances
    def extractConcreteTypes(definitions: Either[String, List[PropMeta[JavaLanguage]]]) =
      definitions.fold[Target[List[PropMeta[JavaLanguage]]]](Target.raiseUserError, Target.pure)

    def protocolImports() =
      (List(
        "com.fasterxml.jackson.annotation.JsonCreator",
        "com.fasterxml.jackson.annotation.JsonIgnoreProperties",
        "com.fasterxml.jackson.annotation.JsonProperty",
        "com.fasterxml.jackson.annotation.JsonValue",
        "javax.validation.constraints.Max",
        "javax.validation.constraints.Min"
      ).map(safeParseRawImport) ++ List(
            "java.util.Objects.requireNonNull"
          ).map(safeParseRawStaticImport)).sequence

    def staticProtocolImports(pkgName: List[String]) =
      Target.pure(List.empty)

    def generateSupportDefinitions() =
      Target.pure(List.empty)

    def packageObjectImports() =
      Target.pure(List.empty)

    def packageObjectContents() =
      Target.pure(List.empty)

    def implicitsObject() =
      Target.pure(None)
  }

  def PolyProtocolTermInterp(
      implicit Cl: CollectionsLibTerms[JavaLanguage, Target],
      Ca: CollectionsAbstraction[JavaLanguage]
  ): PolyProtocolTerms[JavaLanguage, Target] =
    new PolyProtocolTermInterp
  class PolyProtocolTermInterp(implicit Cl: CollectionsLibTerms[JavaLanguage, Target], Ca: CollectionsAbstraction[JavaLanguage])
      extends PolyProtocolTerms[JavaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances

    def renderSealedTrait(
        className: String,
        selfParams: List[ProtocolParameter[JavaLanguage]],
        discriminator: Discriminator[JavaLanguage],
        parents: List[SuperClass[JavaLanguage]],
        children: List[String]
    ) = {
      val parentsWithDiscriminators = parents.collect({ case p if p.discriminators.nonEmpty => p })
      for {
        parentOpt <- (parentsWithDiscriminators, parents) match {
          case _ if parentsWithDiscriminators.length > 1 =>
            Target.raiseUserError[Option[SuperClass[JavaLanguage]]](
              s"${className} requires unsupported multiple inheritance due to multiple parents with discriminators (${parentsWithDiscriminators.map(_.clsName).mkString(", ")})"
            )
          case _ if parentsWithDiscriminators.length == 1 => Target.pure(parentsWithDiscriminators.headOption)
          case _ if parents.length == 1                   => Target.pure(parents.headOption)
          case _                                          => Target.pure(None)
        }

        parentParams                               = parentOpt.toList.flatMap(_.params)
        parentParamNames                           = parentParams.map(_.name.value)
        (parentRequiredTerms, parentOptionalTerms) = sortParams(parentParams)
        parentTerms                                = parentRequiredTerms ++ parentOptionalTerms
        params = parents.filterNot(parent => parentOpt.contains(parent)).flatMap(_.params) ++ selfParams.filterNot(
              param => parentParamNames.contains(param.term.getName.getIdentifier)
            )
        (requiredTerms, optionalTerms) = sortParams(params)
        terms                          = requiredTerms ++ optionalTerms

        abstractClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier, abstractModifier), false, className)
          .addAnnotation(
            new NormalAnnotationExpr(
              new Name("JsonIgnoreProperties"),
              new NodeList(
                new MemberValuePair(
                  "ignoreUnknown",
                  new BooleanLiteralExpr(true)
                )
              )
            )
          )
          .addAnnotation(
            new NormalAnnotationExpr(
              new Name("JsonTypeInfo"),
              new NodeList(
                new MemberValuePair(
                  "use",
                  new FieldAccessExpr(new NameExpr("JsonTypeInfo.Id"), "NAME")
                ),
                new MemberValuePair(
                  "include",
                  new FieldAccessExpr(new NameExpr("JsonTypeInfo.As"), "PROPERTY")
                ),
                new MemberValuePair(
                  "property",
                  new StringLiteralExpr(discriminator.propertyName)
                )
              )
            )
          )
          .addSingleMemberAnnotation(
            "JsonSubTypes",
            new ArrayInitializerExpr(
              new NodeList(
                children.map(
                  child =>
                    new NormalAnnotationExpr(
                      new Name("JsonSubTypes.Type"),
                      new NodeList(
                        new MemberValuePair(
                          "name",
                          new StringLiteralExpr(
                            discriminator.mapping
                              .collectFirst({ case (value, elem) if elem.name == child => value })
                              .getOrElse(child)
                          )
                        ),
                        new MemberValuePair("value", new ClassExpr(StaticJavaParser.parseType(child)))
                      )
                    )
                ): _*
              )
            )
          )

        _ = addParents(abstractClass, parentOpt)

        _ = terms.foreach({ term =>
          val field = abstractClass.addField(term.fieldType, term.parameterName, PRIVATE, FINAL)
          field.addAnnotation(new SingleMemberAnnotationExpr(new Name("JsonProperty"), new StringLiteralExpr(term.propertyName)))
        })

        superCall = new MethodCallExpr("super", parentTerms.map(term => new NameExpr(term.parameterName)): _*)
        constructorBody <- dtoConstructorBody(superCall, requiredTerms ++ optionalTerms)
        _ = abstractClass
          .addConstructor(PROTECTED)
          .setParameters(
            (parentTerms ++ terms)
              .map(term => new Parameter(new NodeList(finalModifier), term.fieldType, new SimpleName(term.parameterName)))
              .toNodeList
          )
          .setBody(constructorBody)

        _ = terms.foreach(addParameterGetter(abstractClass, _))
      } yield abstractClass
    }

    def extractSuperClass(
        swagger: Tracker[ComposedSchema],
        definitions: List[(String, Tracker[Schema[_]])]
    ) = {
      def allParents(model: Tracker[Schema[_]]): List[(String, Tracker[Schema[_]], List[Tracker[Schema[_]]])] =
        model
          .refine({ case c: ComposedSchema => c })(
            _.downField("allOf", _.getAllOf()).indexedDistribute
              .flatMap({ elem =>
                definitions
                  .collectFirst({
                    case (clsName, e) if elem.downField("$ref", _.get$ref()).exists(_.unwrapTracker.endsWith(s"/$clsName")) =>
                      (clsName, e, List.empty) :: allParents(e)
                  })
                  .getOrElse(List.empty)
              })
          )
          .getOrElse(List.empty)

      Target.pure(allParents(swagger))
    }

    def renderADTStaticDefns(
        clsName: String,
        discriminator: Discriminator[JavaLanguage],
        encoder: Option[com.github.javaparser.ast.body.VariableDeclarator],
        decoder: Option[com.github.javaparser.ast.body.VariableDeclarator]
    ) =
      for {
        extraImports <- List(
          "com.fasterxml.jackson.annotation.JsonIgnoreProperties",
          "com.fasterxml.jackson.annotation.JsonSubTypes",
          "com.fasterxml.jackson.annotation.JsonTypeInfo"
        ).traverse(safeParseRawImport)
      } yield StaticDefns[JavaLanguage](
        clsName,
        extraImports,
        List.empty
      )

    def decodeADT(
        clsName: String,
        discriminator: Discriminator[JavaLanguage],
        children: List[String] = Nil
    ) =
      Target.pure(None)

    def encodeADT(
        clsName: String,
        discriminator: Discriminator[JavaLanguage],
        children: List[String] = Nil
    ) =
      Target.pure(None)
  }
}
