package com.twilio.guardrail.protocol.terms.protocol

import cats.Monad
import com.twilio.guardrail.SwaggerUtil.ResolvedType
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.terms.CollectionsLibTerms
import com.twilio.guardrail.{ ProtocolParameter, StaticDefns, SuperClass }
import io.swagger.v3.oas.models.media.Schema

abstract class ModelProtocolTerms[L <: LA, F[_]](implicit Cl: CollectionsLibTerms[L, F]) {
  def MonadF: Monad[F]
  def extractProperties(swagger: Tracker[Schema[_]]): F[List[(String, Tracker[Schema[_]])]]
  def transformProperty(
      clsName: String,
      dtoPackage: List[String],
      supportPackage: List[String],
      concreteTypes: List[PropMeta[L]]
  )(
      name: String,
      fieldName: String,
      prop: Schema[_],
      meta: ResolvedType[L],
      requirement: PropertyRequirement,
      constraints: Set[PropertyConstraint],
      isCustomType: Boolean,
      defaultValue: Option[L#Term]
  ): F[ProtocolParameter[L]]
  def renderDTOClass(clsName: String, supportPackage: List[String], terms: List[ProtocolParameter[L]], parents: List[SuperClass[L]] = Nil): F[L#ClassDefinition]
  def encodeModel(
      clsName: String,
      dtoPackage: List[String],
      params: List[ProtocolParameter[L]],
      parents: List[SuperClass[L]] = Nil
  ): F[Option[L#ValueDefinition]]
  def decodeModel(
      clsName: String,
      dtoPackage: List[String],
      supportPackage: List[String],
      params: List[ProtocolParameter[L]],
      parents: List[SuperClass[L]] = Nil
  ): F[Option[L#ValueDefinition]]
  def renderDTOStaticDefns(clsName: String, deps: List[L#TermName], encoder: Option[L#ValueDefinition], decoder: Option[L#ValueDefinition]): F[StaticDefns[L]]

  def copy(
      newMonadF: Monad[F] = MonadF,
      newExtractProperties: Tracker[Schema[_]] => F[List[(String, Tracker[Schema[_]])]] = extractProperties _,
      newTransformProperty: (
          String,
          List[String],
          List[String],
          List[PropMeta[L]]
      ) => (String, String, Schema[_], ResolvedType[L], PropertyRequirement, Set[PropertyConstraint], Boolean, Option[L#Term]) => F[ProtocolParameter[L]] =
        transformProperty _,
      newRenderDTOClass: (String, List[String], List[ProtocolParameter[L]], List[SuperClass[L]]) => F[L#ClassDefinition] = renderDTOClass _,
      newDecodeModel: (String, List[String], List[String], List[ProtocolParameter[L]], List[SuperClass[L]]) => F[Option[L#ValueDefinition]] = decodeModel _,
      newEncodeModel: (String, List[String], List[ProtocolParameter[L]], List[SuperClass[L]]) => F[Option[L#ValueDefinition]] = encodeModel _,
      newRenderDTOStaticDefns: (String, List[L#TermName], Option[L#ValueDefinition], Option[L#ValueDefinition]) => F[StaticDefns[L]] = renderDTOStaticDefns _
  ) = new ModelProtocolTerms[L, F] {
    def MonadF                                         = newMonadF
    def extractProperties(swagger: Tracker[Schema[_]]) = newExtractProperties(swagger)
    def transformProperty(
        clsName: String,
        dtoPackage: List[String],
        supportPackage: List[String],
        concreteTypes: List[PropMeta[L]]
    )(
        name: String,
        fieldName: String,
        prop: Schema[_],
        meta: ResolvedType[L],
        requirement: PropertyRequirement,
        constraints: Set[PropertyConstraint],
        isCustomType: Boolean,
        defaultValue: Option[L#Term]
    ) =
      newTransformProperty(clsName, dtoPackage, supportPackage, concreteTypes)(
        name,
        fieldName,
        prop,
        meta,
        requirement,
        constraints,
        isCustomType,
        defaultValue
      )
    def renderDTOClass(clsName: String, supportPackage: List[String], terms: List[ProtocolParameter[L]], parents: List[SuperClass[L]] = Nil) =
      newRenderDTOClass(clsName, supportPackage, terms, parents)
    def encodeModel(
        clsName: String,
        dtoPackage: List[String],
        params: List[ProtocolParameter[L]],
        parents: List[SuperClass[L]] = Nil
    ) =
      newEncodeModel(clsName, dtoPackage, params, parents)
    def decodeModel(
        clsName: String,
        dtoPackage: List[String],
        supportPackage: List[String],
        params: List[ProtocolParameter[L]],
        parents: List[SuperClass[L]] = Nil
    ) =
      newDecodeModel(clsName, dtoPackage, supportPackage, params, parents)
    def renderDTOStaticDefns(clsName: String, deps: List[L#TermName], encoder: Option[L#ValueDefinition], decoder: Option[L#ValueDefinition]) =
      newRenderDTOStaticDefns(clsName, deps, encoder, decoder)
  }
}
