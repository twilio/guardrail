package com.twilio.guardrail.generators.collections

import cats.Monad
import com.github.javaparser.ast.{ Node, NodeList }
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, Type }
import com.twilio.guardrail.{ SwaggerUtil, Target }
import com.twilio.guardrail.generators.syntax.Java.{ buildMethodCall, _ }
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.terms.CollectionsLibTerms

object JavaVavrCollectionsGenerator {
  class JavaVavrCollectionsInterp extends CollectionsLibTerms[JavaLanguage, Target] {
    private val baseInterp = new JavaCollectionsGenerator.JavaCollectionsInterp

    override def MonadF: Monad[Target] = Target.targetInstances

    override def vendorPrefixes(): Target[List[String]] = baseInterp.vendorPrefixes().map("x-java-vavr" +: _)

    override def liftOptionalType(value: Type): Target[Type] =
      safeParseClassOrInterfaceType("io.vavr.control.Option").map(_.setTypeArguments(new NodeList(value)))

    override def liftOptionalTerm(value: Node): Target[Node] =
      buildMethodCall("io.vavr.control.Option.of", Some(value))

    override def liftSomeTerm(value: Node): Target[Node] =
      for {
        wrappedValue <- buildMethodCall("java.util.Objects.requireNonNull", Some(value))
        term         <- buildMethodCall("io.vavr.control.Option.some", Some(wrappedValue))
      } yield term

    override def emptyOptionalTerm(): Target[Node] = buildMethodCall("io.vavr.control.Option.none")

    private def liftSimpleType(value: Type, tpeStr: String, customTpe: Option[Type]): Target[Type] =
      customTpe
        .fold[Target[ClassOrInterfaceType]](safeParseClassOrInterfaceType(tpeStr))({
          case t: ClassOrInterfaceType =>
            Target.pure(t)
          case x =>
            Target.raiseUserError(s"Unsure how to map $x")
        })
        .map(_.setTypeArguments(new NodeList(value)))

    override def liftArrayType(value: Type, customTpe: Option[Type]): Target[Type] =
      liftSimpleType(value, "io.vavr.collection.Vector", customTpe)

    override def liftVectorType(value: Type, customTpe: Option[Type]): Target[Type] =
      liftSimpleType(value, "io.vavr.collection.Vector", customTpe)

    override def liftVectorTerm(value: Node): Target[Node] =
      buildMethodCall("io.vavr.collection.Vector.of", Some(value))

    override def emptyArray(): Target[Node] = buildMethodCall("io.vavr.collection.Vector.empty")

    override def embedArray(tpe: SwaggerUtil.LazyResolvedType[JavaLanguage], containerTpe: Option[Type]): Target[SwaggerUtil.LazyResolvedType[JavaLanguage]] =
      tpe match {
        case SwaggerUtil.Deferred(tpe) =>
          Target.pure(SwaggerUtil.DeferredArray[JavaLanguage](tpe, containerTpe))
        case SwaggerUtil.DeferredArray(_, _) =>
          Target.raiseUserError("FIXME: Got an Array of Arrays, currently not supported")
        case SwaggerUtil.DeferredMap(_, _) =>
          Target.raiseUserError("FIXME: Got an Array of Maps, currently not supported")
      }

    override def liftMapType(value: Type, customTpe: Option[Type]): Target[Type] =
      customTpe
        .fold[Target[ClassOrInterfaceType]](safeParseClassOrInterfaceType("io.vavr.collection.Map").map(identity))({
          case t: ClassOrInterfaceType =>
            Target.pure(t)
          case x =>
            Target.raiseUserError(s"Unsure how to map $x")
        })
        .map(_.setTypeArguments(STRING_TYPE, value))

    override def emptyMap(): Target[Node] =
      buildMethodCall("io.vavr.collection.HashMap.empty")

    override def embedMap(tpe: SwaggerUtil.LazyResolvedType[JavaLanguage], containerTpe: Option[Type]): Target[SwaggerUtil.LazyResolvedType[JavaLanguage]] =
      tpe match {
        case SwaggerUtil.Deferred(inner) =>
          Target.pure(SwaggerUtil.DeferredMap[JavaLanguage](inner, containerTpe))
        case SwaggerUtil.DeferredMap(_, _) =>
          Target.raiseUserError("FIXME: Got a map of maps, currently not supported")
        case SwaggerUtil.DeferredArray(_, _) =>
          Target.raiseUserError("FIXME: Got a map of arrays, currently not supported")
      }
  }
}
