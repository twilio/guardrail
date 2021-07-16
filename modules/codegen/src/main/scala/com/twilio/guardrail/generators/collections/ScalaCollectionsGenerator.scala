package com.twilio.guardrail.generators.collections

import cats.Monad
import com.twilio.guardrail.SwaggerUtil.LazyResolvedType
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.terms.CollectionsLibTerms
import com.twilio.guardrail.{ SwaggerUtil, Target }
import scala.meta._

object ScalaCollectionsGenerator {
  object ScalaCollectionsInterp extends CollectionsLibTerms[ScalaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances

    def vendorPrefixes(): Target[List[String]] = Target.pure(List("x-scala", "x-jvm"))

    def liftOptionalType(value: Type): Target[Type] = Target.pure(t"Option[$value]")
    def liftOptionalTerm(value: Term): Target[Term] = Target.pure(q"Option($value)")
    def liftSomeTerm(value: Term): Target[Term]     = Target.pure(q"Some($value)")
    def emptyOptionalTerm(): Target[Term]           = Target.pure(q"None")

    def liftArrayType(value: Type, customTpe: Option[Type]): Target[Type] =
      Target.pure(t"${customTpe.getOrElse(t"Iterable")}[$value]")
    def liftVectorType(value: Type, customTpe: Option[Type]): Target[Type] =
      Target.pure(t"${customTpe.getOrElse(t"Vector")}[$value]")
    def liftVectorTerm(value: Term): Target[Term] = Target.pure(q"Vector($value)")
    def emptyArray(): Target[Term]                = Target.pure(q"Vector.empty")
    def embedArray(tpe: LazyResolvedType[ScalaLanguage], containerTpe: Option[Type]): Target[LazyResolvedType[ScalaLanguage]] = tpe match {
      case SwaggerUtil.Deferred(tpe) =>
        Target.pure(SwaggerUtil.DeferredArray[ScalaLanguage](tpe, containerTpe))
      case SwaggerUtil.DeferredArray(_, _) =>
        Target.raiseUserError("FIXME: Got an Array of Arrays, currently not supported")
      case SwaggerUtil.DeferredMap(_, _) =>
        Target.raiseUserError("FIXME: Got an Array of Maps, currently not supported")
    }

    def liftMapType(value: Type, customTpe: Option[Type]): Target[Type] =
      Target.pure(t"${customTpe.getOrElse(t"Map")}[String, $value]")
    def emptyMap(): Target[Term] = Target.pure(q"Map.empty")
    def embedMap(tpe: LazyResolvedType[ScalaLanguage], containerTpe: Option[Type]): Target[LazyResolvedType[ScalaLanguage]] = tpe match {
      case SwaggerUtil.Deferred(inner) =>
        Target.pure(SwaggerUtil.DeferredMap[ScalaLanguage](inner, containerTpe))
      case SwaggerUtil.DeferredMap(_, _) =>
        Target.raiseUserError("FIXME: Got a map of maps, currently not supported")
      case SwaggerUtil.DeferredArray(_, _) =>
        Target.raiseUserError("FIXME: Got a map of arrays, currently not supported")
    }
  }
}
