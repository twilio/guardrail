package com.twilio.guardrail

import _root_.io.swagger.v3.oas.models._
import _root_.io.swagger.v3.oas.models.media._
import cats.data.NonEmptyList
import cats.free.Free
import cats.implicits._
import com.twilio.guardrail.core.{ Mappish, Tracker }
import com.twilio.guardrail.core.implicits._
import com.twilio.guardrail.generators.RawParameterType
import com.twilio.guardrail.generators.syntax._
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.protocol.terms.protocol._
import com.twilio.guardrail.terms.framework.FrameworkTerms
import com.twilio.guardrail.terms.{ ScalaTerms, SwaggerTerms }
import java.util.Locale

import cats.Foldable
import com.twilio.guardrail.extract.Default

import scala.collection.JavaConverters._
import scala.language.higherKinds

case class ProtocolDefinitions[L <: LA](
    elems: List[StrictProtocolElems[L]],
    protocolImports: List[L#Import],
    packageObjectImports: List[L#Import],
    packageObjectContents: List[L#ValueDefinition]
)
sealed trait EmptyToNullBehaviour
case object EmptyIsNull  extends EmptyToNullBehaviour
case object EmptyIsEmpty extends EmptyToNullBehaviour

sealed trait RedactionBehaviour
case object DataVisible  extends RedactionBehaviour
case object DataRedacted extends RedactionBehaviour

case class ProtocolParameter[L <: LA](
    term: L#MethodParameter,
    name: String,
    dep: Option[L#TermName],
    rawType: RawParameterType,
    readOnlyKey: Option[String],
    emptyToNull: EmptyToNullBehaviour,
    dataRedaction: RedactionBehaviour,
    defaultValue: Option[L#Term]
)

case class Discriminator[L <: LA](propertyName: String, mapping: Map[String, ProtocolElems[L]])

object Discriminator {
  def fromSchema[L <: LA, F[_]](schema: Schema[_])(implicit Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F]): Free[F, Option[Discriminator[L]]] =
    Sw.log.function("Discriminator.fromSchema") {
      import Sc._
      Option(schema.getDiscriminator)
        .flatMap(x => Option(x.getPropertyName).map((x, _)))
        .traverse {
          case (x, propertyName) =>
            val possibleMappings = Option(x.getMapping)
              .map(_.asScala)
              .getOrElse(Map.empty[String, String])
              .flatMap({
                case (k, s) if s.startsWith("#/") => s.split("/").lastOption.filter(_.nonEmpty).map((k, _))
                case (k, s)                       => Option((k, s))
              })
              .toList
            for {
              mapping <- possibleMappings.flatTraverse({
                case (key, name) =>
                  parseType(name).map(_.map(tpe => (key, RandomType(name, tpe))).toList)
              })
            } yield Discriminator[L](propertyName, mapping.toMap)
        }
    }
}

case class SuperClass[L <: LA](
    clsName: String,
    tpl: L#TypeName,
    interfaces: List[String],
    params: List[ProtocolParameter[L]],
    discriminators: List[Discriminator[L]]
)

object ProtocolGenerator {
  private[this] def getRequiredFieldsRec(root: Tracker[Schema[_]]): List[String] = {
    @scala.annotation.tailrec
    def work(values: List[Tracker[Schema[_]]], acc: List[String]): List[String] = {
      val required = values.flatMap(value => value.downField("required", _.getRequired()).get)
      val next: List[Tracker[Schema[_]]] =
        for {
          a <- values
          b <- a.refine({ case x: ComposedSchema => x })(_.downField("allOf", _.getAllOf())).toOption.toList
          c <- b.indexedDistribute
        } yield c

      val newRequired = acc ++ required

      next match {
        case next @ (_ :: _) => work(next, newRequired)
        case Nil             => newRequired
      }
    }
    work(List(root), Nil)
  }

  private[this] def fromEnum[L <: LA, F[_]](
      clsName: String,
      swagger: Tracker[Schema[_]],
      dtoPackage: List[String]
  )(implicit E: EnumProtocolTerms[L, F], F: FrameworkTerms[L, F], Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F]): Free[F, Either[String, EnumDefinition[L]]] = {
    import E._
    import Sc._

    def validProg(enum: List[String], tpe: L#Type, fullType: L#Type): Free[F, EnumDefinition[L]] =
      for {
        elems <- enum.traverse { elem =>
          for {
            termName  <- formatEnumName(elem)
            valueTerm <- pureTermName(termName)
            accessor  <- buildAccessor(clsName, termName)
          } yield (elem, valueTerm, accessor)
        }
        pascalValues = elems.map(_._2)
        members <- renderMembers(clsName, elems)
        encoder <- encodeEnum(clsName)
        decoder <- decodeEnum(clsName)

        defn        <- renderClass(clsName, tpe, elems)
        staticDefns <- renderStaticDefns(clsName, members, pascalValues, encoder, decoder)
        classType   <- pureTypeName(clsName)
      } yield EnumDefinition[L](clsName, classType, fullType, elems, defn, staticDefns)

    // Default to `string` for untyped enums.
    // Currently, only plain strings are correctly supported anyway, so no big loss.
    val tpeName = swagger.downField("type", _.getType()).map(_.filterNot(_ == "object").orElse(Option("string")))

    for {
      enum          <- extractEnum(swagger.get)
      customTpeName <- SwaggerUtil.customTypeName(swagger)
      tpe           <- SwaggerUtil.typeName(tpeName, swagger.downField("format", _.getFormat()), customTpeName)
      fullType      <- selectType(NonEmptyList.fromList(dtoPackage :+ clsName).getOrElse(NonEmptyList.of(clsName)))
      res           <- enum.traverse(validProg(_, tpe, fullType))
    } yield res
  }

  /**
    * types of things we can losslessly convert between snake and camel case:
    *   - foo
    *   - foo_bar
    *   - foo_bar_baz
    *   - foo.bar
    *
    * types of things we canNOT losslessly convert between snake and camel case:
    *   - Foo
    *   - Foo_bar
    *   - Foo_Bar
    *   - FooBar
    *   - foo_barBaz
    *
    * so essentially we have to return false if:
    *   - there are any uppercase characters
    */
  def couldBeSnakeCase(s: String): Boolean = s.toLowerCase(Locale.US) == s

  /**
    * Handle polymorphic model
    */
  private[this] def fromPoly[L <: LA, F[_]](
      hierarchy: ClassParent[L],
      concreteTypes: List[PropMeta[L]],
      definitions: List[(String, Tracker[Schema[_]])],
      dtoPackage: List[String]
  )(
      implicit F: FrameworkTerms[L, F],
      P: PolyProtocolTerms[L, F],
      E: EnumProtocolTerms[L, F],
      M: ModelProtocolTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, ProtocolElems[L]] = {
    import M._
    import P._
    import Sc._

    def child(hierarchy: ClassHierarchy[L]): List[String] =
      hierarchy.children.map(_.name) ::: hierarchy.children.flatMap(child)
    def parent(hierarchy: ClassHierarchy[L]): List[String] =
      if (hierarchy.children.nonEmpty) hierarchy.name :: hierarchy.children.flatMap(parent)
      else Nil

    val children      = child(hierarchy).diff(parent(hierarchy)).distinct
    val discriminator = hierarchy.discriminator

    for {
      parents <- hierarchy.model
        .refine[Free[F, List[SuperClass[L]]]]({ case c: ComposedSchema => c })(extractParents(_, definitions, concreteTypes, dtoPackage))
        .getOrElse(Free.pure[F, List[SuperClass[L]]](Nil))
      props <- extractProperties(hierarchy.model)
      requiredFields           = hierarchy.required ::: hierarchy.children.flatMap(_.required)
      needCamelSnakeConversion = props.forall { case (k, _) => couldBeSnakeCase(k) }
      params <- props.traverse({
        case (name, prop) =>
          val typeName   = NonEmptyList.of(hierarchy.name, name.toCamelCase.capitalize)
          val isRequired = requiredFields.contains(name)
          for {
            customType   <- SwaggerUtil.customTypeName(prop)
            resolvedType <- SwaggerUtil.propMeta[L, F](prop)
            defValue     <- defaultValue(typeName, prop.get, isRequired, definitions.map(_.map(_.get)))
            res <- transformProperty(hierarchy.name, needCamelSnakeConversion, concreteTypes)(
              name,
              prop.get,
              resolvedType,
              isRequired,
              customType.isDefined,
              defValue
            )
          } yield res
      })
      definition  <- renderSealedTrait(hierarchy.name, params, discriminator, parents, children)
      encoder     <- encodeADT(hierarchy.name, hierarchy.discriminator, children)
      decoder     <- decodeADT(hierarchy.name, hierarchy.discriminator, children)
      staticDefns <- renderADTStaticDefns(hierarchy.name, discriminator, encoder, decoder)
      tpe         <- pureTypeName(hierarchy.name)
      fullType    <- selectType(NonEmptyList.fromList(dtoPackage :+ hierarchy.name).getOrElse(NonEmptyList.of(hierarchy.name)))
    } yield {
      ADT[L](
        name = hierarchy.name,
        tpe = tpe,
        fullType = fullType,
        trt = definition,
        staticDefns = staticDefns
      )
    }
  }

  def extractParents[L <: LA, F[_]](
      elem: Tracker[ComposedSchema],
      definitions: List[(String, Tracker[Schema[_]])],
      concreteTypes: List[PropMeta[L]],
      dtoPackage: List[String]
  )(
      implicit M: ModelProtocolTerms[L, F],
      F: FrameworkTerms[L, F],
      E: EnumProtocolTerms[L, F],
      P: PolyProtocolTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, List[SuperClass[L]]] = {
    import M._
    import P._
    import Sc._

    for {
      a <- extractSuperClass(elem, definitions)
      supper <- a.flatTraverse {
        case (clsName, _extends, interfaces) =>
          val concreteInterfacesWithClass = for {
            interface      <- interfaces
            (cls, tracker) <- definitions
            result <- tracker
              .refine[Tracker[Schema[_]]]({
                case x: ComposedSchema if interface.downField("$ref", _.get$ref()).exists(_.get.endsWith(s"/${cls}")) => x
              })(
                identity _
              )
              .orRefine({ case x: Schema[_] if interface.downField("$ref", _.get$ref()).exists(_.get.endsWith(s"/${cls}")) => x })(identity _)
              .toOption
          } yield (cls -> result)
          val (_, concreteInterfaces) = concreteInterfacesWithClass.unzip
          val classMapping = (for {
            (cls, schema) <- concreteInterfacesWithClass
            (name, _)     <- schema.downField("properties", _.getProperties).indexedDistribute.value
          } yield (name, cls)).toMap
          for {
            _extendsProps <- extractProperties(_extends)
            requiredFields = getRequiredFieldsRec(_extends) ++ concreteInterfaces.flatMap(getRequiredFieldsRec)
            _withProps <- concreteInterfaces.traverse(extractProperties)
            props = _extendsProps ++ _withProps.flatten
            (params, _) <- prepareProperties(
              NonEmptyList.of(clsName),
              classMapping,
              props,
              requiredFields,
              concreteTypes,
              definitions,
              dtoPackage
            )
            interfacesCls = interfaces.flatMap(_.downField("$ref", _.get$ref).map(_.map(_.split("/").last)).get)
            tpe <- parseTypeName(clsName)

            discriminators <- (_extends :: concreteInterfaces).flatTraverse(
              _.refine[Free[F, List[Discriminator[L]]]]({ case m: ObjectSchema => m })(m => Discriminator.fromSchema(m.get).map(_.toList))
                .getOrElse(Free.pure[F, List[Discriminator[L]]](List.empty))
            )
          } yield tpe
            .map(
              SuperClass[L](
                clsName,
                _,
                interfacesCls,
                params,
                discriminators
              )
            )
            .toList
      }

    } yield supper
  }

  private[this] def fromModel[L <: LA, F[_]](
      clsName: NonEmptyList[String],
      model: Tracker[Schema[_]],
      parents: List[SuperClass[L]],
      concreteTypes: List[PropMeta[L]],
      definitions: List[(String, Tracker[Schema[_]])],
      dtoPackage: List[String]
  )(
      implicit M: ModelProtocolTerms[L, F],
      F: FrameworkTerms[L, F],
      E: EnumProtocolTerms[L, F],
      P: PolyProtocolTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, Either[String, ClassDefinition[L]]] = {
    import M._
    import Sc._

    for {
      props <- extractProperties(model)
      requiredFields           = getRequiredFieldsRec(model)
      needCamelSnakeConversion = props.forall { case (k, _) => couldBeSnakeCase(k) }
      (params, nestedDefinitions) <- prepareProperties(clsName, Map.empty, props, requiredFields, concreteTypes, definitions, dtoPackage)
      defn                        <- renderDTOClass(clsName.last, params, parents)
      encoder                     <- encodeModel(clsName.last, needCamelSnakeConversion, params, parents)
      decoder                     <- decodeModel(clsName.last, needCamelSnakeConversion, params, parents)
      tpe                         <- parseTypeName(clsName.last)
      fullType                    <- selectType(dtoPackage.foldRight(clsName)((x, xs) => xs.prepend(x)))
      staticDefns                 <- renderDTOStaticDefns(clsName.last, List.empty, encoder, decoder)
      result <- if (parents.isEmpty && props.isEmpty) Free.pure[F, Either[String, ClassDefinition[L]]](Left("Entity isn't model"))
      else {
        val nestedClasses = nestedDefinitions.flatTraverse {
          case classDefinition: ClassDefinition[L] =>
            for {
              widenClass          <- widenClassDefinition(classDefinition.cls)
              companionTerm       <- pureTermName(classDefinition.name)
              companionDefinition <- wrapToObject(companionTerm, classDefinition.staticDefns.extraImports, classDefinition.staticDefns.definitions)
              widenCompanion      <- companionDefinition.traverse(widenObjectDefinition)
            } yield List(widenClass) ++ widenCompanion
          case enumDefinition: EnumDefinition[L] =>
            for {
              widenClass          <- widenClassDefinition(enumDefinition.cls)
              companionTerm       <- pureTermName(enumDefinition.name)
              companionDefinition <- wrapToObject(companionTerm, enumDefinition.staticDefns.extraImports, enumDefinition.staticDefns.definitions)
              widenCompanion      <- companionDefinition.traverse(widenObjectDefinition)
            } yield List(widenClass) ++ widenCompanion
        }
        nestedClasses.map { v =>
          val finalStaticDefns = staticDefns.copy(definitions = staticDefns.definitions ++ v)
          tpe.toRight("Empty entity name").map(ClassDefinition[L](clsName.last, _, fullType, defn, finalStaticDefns, parents))
        }
      }
    } yield result
  }

  private def prepareProperties[L <: LA, F[_]](
      clsName: NonEmptyList[String],
      propertyToTypeLookup: Map[String, String],
      props: List[(String, Tracker[Schema[_]])],
      requiredFields: List[String],
      concreteTypes: List[PropMeta[L]],
      definitions: List[(String, Tracker[Schema[_]])],
      dtoPackage: List[String]
  )(
      implicit M: ModelProtocolTerms[L, F],
      F: FrameworkTerms[L, F],
      E: EnumProtocolTerms[L, F],
      P: PolyProtocolTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, (List[ProtocolParameter[L]], List[NestedProtocolElems[L]])] = {
    import M._
    import Sc._
    def getClsName(name: String): NonEmptyList[String] = propertyToTypeLookup.get(name).map(NonEmptyList.of(_)).getOrElse(clsName)

    def processProperty(name: String, schema: Tracker[Schema[_]]): Free[F, Option[Either[String, NestedProtocolElems[L]]]] = {
      val nestedClassName = getClsName(name).append(name.toCamelCase.capitalize)
      schema
        .refine[Free[F, Option[Either[String, NestedProtocolElems[L]]]]]({ case x: ObjectSchema => x })(
          _ => fromModel(nestedClassName, schema, List.empty, concreteTypes, definitions, dtoPackage).map(Option(_))
        )
        .orRefine({ case o: ComposedSchema => o })(
          o =>
            for {
              parents              <- extractParents(o, definitions, concreteTypes, dtoPackage)
              maybeClassDefinition <- fromModel(nestedClassName, schema, parents, concreteTypes, definitions, dtoPackage)
            } yield Option(maybeClassDefinition)
        )
        .orRefine({ case a: ArraySchema => a })(_.downField("items", _.getItems()).indexedCosequence.flatTraverse(processProperty(name, _)))
        .orRefine({ case s: StringSchema if Option(s.getEnum).map(_.asScala).exists(_.nonEmpty) => s })(
          s => fromEnum(nestedClassName.last, s, dtoPackage).map(Option(_))
        )
        .getOrElse(Free.pure[F, Option[Either[String, NestedProtocolElems[L]]]](Option.empty))
    }
    val needCamelSnakeConversion = props.forall { case (k, _) => couldBeSnakeCase(k) }
    for {
      paramsAndNestedDefinitions <- props.traverse[Free[F, ?], (Tracker[ProtocolParameter[L]], Option[NestedProtocolElems[L]])] {
        case (name, schema) =>
          val typeName = getClsName(name).append(name.toCamelCase.capitalize)
          for {
            tpe                   <- selectType(typeName)
            maybeNestedDefinition <- processProperty(name, schema)
            resolvedType          <- SwaggerUtil.propMetaWithName(tpe, schema)
            customType            <- SwaggerUtil.customTypeName(schema.get)
            isRequired = requiredFields.contains(name)
            defValue <- defaultValue(typeName, schema.get, isRequired, definitions.map(_.map(_.get)))
            parameter <- transformProperty(getClsName(name).last, needCamelSnakeConversion, concreteTypes)(
              name,
              schema.get,
              resolvedType,
              isRequired,
              customType.isDefined,
              defValue
            )
          } yield (Tracker.cloneHistory(schema, parameter), maybeNestedDefinition.flatMap(_.toOption))
      }
      (params, nestedDefinitions) = paramsAndNestedDefinitions.unzip
      deduplicatedParams <- deduplicateParams(params)
    } yield deduplicatedParams -> nestedDefinitions.flatten
  }

  private def deduplicateParams[L <: LA, F[_]](
      params: List[Tracker[ProtocolParameter[L]]]
  )(implicit Sw: SwaggerTerms[L, F], Sc: ScalaTerms[L, F]): Free[F, List[ProtocolParameter[L]]] = {
    import Sc._
    Foldable[List]
      .foldLeftM[Free[F, ?], Tracker[ProtocolParameter[L]], List[ProtocolParameter[L]]](params, List.empty[ProtocolParameter[L]]) { (s, ta) =>
        val a = ta.get
        s.find(p => p.name == a.name) match {
          case None => Free.pure(a :: s)
          case Some(duplicate) =>
            for {
              newDefaultValue <- findCommonDefaultValue(ta.showHistory, a.defaultValue, duplicate.defaultValue)
              newRawType      <- findCommonRawType(ta.showHistory, a.rawType, duplicate.rawType)
            } yield {
              val emptyToNull        = if (Set(a.emptyToNull, duplicate.emptyToNull).contains(EmptyIsNull)) EmptyIsNull else EmptyIsEmpty
              val redactionBehaviour = if (Set(a.dataRedaction, duplicate.dataRedaction).contains(DataRedacted)) DataRedacted else DataVisible
              val mergedParameter = ProtocolParameter[L](
                a.term,
                a.name,
                a.dep,
                newRawType,
                a.readOnlyKey.orElse(duplicate.readOnlyKey),
                emptyToNull,
                redactionBehaviour,
                newDefaultValue
              )
              mergedParameter :: s.filter(_.name != a.name)
            }
        }
      }
      .map(_.reverse)
  }

  def modelTypeAlias[L <: LA, F[_]](clsName: String, abstractModel: Tracker[Schema[_]])(
      implicit
      Fw: FrameworkTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, ProtocolElems[L]] = {
    import Fw._
    val model: Option[Tracker[ObjectSchema]] = abstractModel
      .refine[Option[Tracker[ObjectSchema]]]({ case m: ObjectSchema => m })(x => Option(x))
      .orRefine({ case m: ComposedSchema => m })(
        _.downField("allOf", _.getAllOf()).indexedCosequence
          .get(1)
          .flatMap(
            _.refine({ case o: ObjectSchema => o })(Option.apply)
              .orRefineFallback(_ => None)
          )
      )
      .orRefineFallback(_ => None)
    for {
      tpe <- model.fold[Free[F, L#Type]](objectType(None)) { m =>
        val raw = m.downField("type", _.getType())
        for {
          tpeName <- SwaggerUtil.customTypeName[L, F, Tracker[ObjectSchema]](m)
          res <- SwaggerUtil.typeName[L, F](
            raw,
            m.downField("format", _.getFormat()),
            tpeName
          )
        } yield res
      }
      res <- typeAlias[L, F](clsName, tpe)
    } yield res
  }

  def plainTypeAlias[L <: LA, F[_]](
      clsName: String
  )(implicit Fw: FrameworkTerms[L, F], Sc: ScalaTerms[L, F]): Free[F, ProtocolElems[L]] = {
    import Fw._
    for {
      tpe <- objectType(None)
      res <- typeAlias[L, F](clsName, tpe)
    } yield res
  }

  def typeAlias[L <: LA, F[_]](clsName: String, tpe: L#Type): Free[F, ProtocolElems[L]] =
    Free.pure(RandomType[L](clsName, tpe))

  def fromArray[L <: LA, F[_]](clsName: String, arr: Tracker[ArraySchema], concreteTypes: List[PropMeta[L]])(
      implicit R: ArrayProtocolTerms[L, F],
      F: FrameworkTerms[L, F],
      P: ProtocolSupportTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, ProtocolElems[L]] = {
    import R._
    for {
      deferredTpe <- SwaggerUtil.modelMetaType(arr)
      tpe         <- extractArrayType(deferredTpe, concreteTypes)
      ret         <- typeAlias[L, F](clsName, tpe)
    } yield ret
  }

  sealed trait ClassHierarchy[L <: LA] {
    def name: String
    def model: Tracker[Schema[_]]
    def children: List[ClassChild[L]]
    def required: List[String]
  }
  case class ClassChild[L <: LA](name: String, model: Tracker[Schema[_]], children: List[ClassChild[L]], required: List[String]) extends ClassHierarchy[L]
  case class ClassParent[L <: LA](
      name: String,
      model: Tracker[Schema[_]],
      children: List[ClassChild[L]],
      discriminator: Discriminator[L],
      required: List[String]
  ) extends ClassHierarchy[L]

  /**
    * returns objects grouped into hierarchies
    */
  def groupHierarchies[L <: LA, F[_]](
      definitions: Mappish[List, String, Tracker[Schema[_]]]
  )(implicit Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F]): Free[F, (List[ClassParent[L]], List[(String, Tracker[Schema[_]])])] = {

    def firstInHierarchy(model: Tracker[Schema[_]]): Option[ObjectSchema] =
      model
        .refine({ case x: ComposedSchema => x })({ elem =>
          definitions.value
            .collectFirst({
              case (clsName, element) if elem.downField("allOf", _.getAllOf).exists(_.downField("$ref", _.get$ref()).exists(_.get.endsWith(s"/$clsName"))) =>
                element
            })
            .flatMap(
              _.refine({ case x: ComposedSchema => x })(firstInHierarchy)
                .orRefine({ case o: ObjectSchema => o })(x => Option(x.get))
                .getOrElse(None)
            )
        })
        .getOrElse(None)

    def children(cls: String): List[ClassChild[L]] = definitions.value.flatMap {
      case (clsName, comp) =>
        comp
          .refine({ case x: ComposedSchema => x })(
            comp =>
              if (comp
                    .downField("allOf", _.getAllOf())
                    .exists(x => x.downField("$ref", _.get$ref()).exists(_.get.endsWith(s"/$cls")))) {
                Some(ClassChild(clsName, comp, children(clsName), getRequiredFieldsRec(comp)))
              } else None
          )
          .getOrElse(None)
    }

    def classHierarchy(cls: String, model: Tracker[Schema[_]]): Free[F, Option[ClassParent[L]]] =
      model
        .refine({ case c: ComposedSchema => c })(
          c =>
            firstInHierarchy(c)
              .fold(Free.pure[F, Option[Discriminator[L]]](Option.empty))(Discriminator.fromSchema)
              .map(_.map((_, getRequiredFieldsRec(c))))
        )
        .orRefine({ case x: Schema[_] => x })(m => Discriminator.fromSchema(m.get).map(_.map((_, getRequiredFieldsRec(m)))))
        .getOrElse(Free.pure[F, Option[(Discriminator[L], List[String])]](Option.empty))
        .map(_.map({ case (discriminator, reqFields) => ClassParent(cls, model, children(cls), discriminator, reqFields) }))

    Sw.log.function("groupHierarchies")(
      definitions.value
        .traverse({
          case (cls, model) =>
            for {
              hierarchy <- classHierarchy(cls, model)
            } yield hierarchy.filterNot(_.children.isEmpty).toLeft((cls, model))
        })
        .map(_.partitionEither[ClassParent[L], (String, Tracker[Schema[_]])](identity))
    )
  }

  def fromSwagger[L <: LA, F[_]](swagger: Tracker[OpenAPI], dtoPackage: List[String])(
      implicit E: EnumProtocolTerms[L, F],
      M: ModelProtocolTerms[L, F],
      R: ArrayProtocolTerms[L, F],
      S: ProtocolSupportTerms[L, F],
      F: FrameworkTerms[L, F],
      P: PolyProtocolTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, ProtocolDefinitions[L]] = {
    import S._
    import Sw._

    val definitions = swagger.downField("components", _.getComponents()).flatDownField("schemas", _.getSchemas()).indexedCosequence
    Sw.log.function("ProtocolGenerator.fromSwagger")(for {
      (hierarchies, definitionsWithoutPoly) <- groupHierarchies(definitions)

      concreteTypes <- SwaggerUtil.extractConcreteTypes[L, F](definitions.value)
      polyADTs      <- hierarchies.traverse(fromPoly(_, concreteTypes, definitions.value, dtoPackage))
      elems <- definitionsWithoutPoly.traverse {
        case (clsName, model) =>
          model
            .refine({ case m: StringSchema => m })(
              m =>
                for {
                  enum  <- fromEnum(clsName, m, dtoPackage)
                  model <- fromModel(NonEmptyList.of(clsName), m, List.empty, concreteTypes, definitions.value, dtoPackage)
                  alias <- modelTypeAlias(clsName, m)
                } yield enum.orElse(model).getOrElse(alias)
            )
            .orRefine({ case c: ComposedSchema => c })(
              comp =>
                for {
                  parents <- extractParents(comp, definitions.value, concreteTypes, dtoPackage)
                  model   <- fromModel(NonEmptyList.of(clsName), comp, parents, concreteTypes, definitions.value, dtoPackage)
                  alias   <- modelTypeAlias(clsName, comp)
                } yield model.getOrElse(alias)
            )
            .orRefine({ case a: ArraySchema => a })(arr => fromArray(clsName, arr, concreteTypes))
            .orRefine({ case o: ObjectSchema => o })(
              m =>
                for {
                  enum  <- fromEnum(clsName, m, dtoPackage)
                  model <- fromModel(NonEmptyList.of(clsName), m, List.empty, concreteTypes, definitions.value, dtoPackage)
                  alias <- modelTypeAlias(clsName, m)
                } yield enum.orElse(model).getOrElse(alias)
            )
            .valueOr(
              x =>
                for {
                  tpeName        <- getType(x)
                  customTypeName <- SwaggerUtil.customTypeName(x.get)
                  tpe            <- SwaggerUtil.typeName[L, F](tpeName.map(Option(_)), x.downField("format", _.getFormat()), customTypeName)
                  res            <- typeAlias(clsName, tpe)
                } yield res
            )
      }
      protoImports      <- protocolImports()
      pkgImports        <- packageObjectImports()
      pkgObjectContents <- packageObjectContents()

      polyADTElems <- ProtocolElems.resolve[L, F](polyADTs)
      strictElems  <- ProtocolElems.resolve[L, F](elems)
    } yield ProtocolDefinitions[L](strictElems ++ polyADTElems, protoImports, pkgImports, pkgObjectContents))
  }

  private def defaultValue[L <: LA, F[_]](name: NonEmptyList[String], schema: Schema[_], isRequired: Boolean, definitions: List[(String, Schema[_])])(
      implicit Sc: ScalaTerms[L, F]
  ): Free[F, Option[L#Term]] = {
    import Sc._
    val empty = Free.pure[F, Option[L#Term]](None)
    Option(schema.get$ref()) match {
      case Some(ref) =>
        definitions
          .collectFirst {
            case (cls, refSchema) if ref.endsWith(s"/$cls") =>
              defaultValue(NonEmptyList.of(cls), refSchema, isRequired, definitions)
          }
          .getOrElse(Free.pure(None))
      case None =>
        schema match {
          case map: MapSchema if isRequired =>
            for {
              customTpe <- SwaggerUtil.customMapTypeName(map)
              result    <- customTpe.fold(emptyMap.map(Option(_)))(_ => Free.pure(None))
            } yield result
          case arr: ArraySchema if isRequired =>
            for {
              customTpe <- SwaggerUtil.customArrayTypeName(arr)
              result    <- customTpe.fold(emptyArray.map(Option(_)))(_ => Free.pure(None))
            } yield result
          case p: BooleanSchema =>
            Default(p).extract[Boolean].fold(empty)(litBoolean(_).map(Some(_)))
          case p: NumberSchema if p.getFormat == "double" =>
            Default(p).extract[Double].fold(empty)(litDouble(_).map(Some(_)))
          case p: NumberSchema if p.getFormat == "float" =>
            Default(p).extract[Float].fold(empty)(litFloat(_).map(Some(_)))
          case p: IntegerSchema if p.getFormat == "int32" =>
            Default(p).extract[Int].fold(empty)(litInt(_).map(Some(_)))
          case p: IntegerSchema if p.getFormat == "int64" =>
            Default(p).extract[Long].fold(empty)(litLong(_).map(Some(_)))
          case p: StringSchema if Option(p.getEnum).map(_.asScala).exists(_.nonEmpty) =>
            Default(p).extract[String] match {
              case Some(defaultEnumValue) =>
                for {
                  enumName <- formatEnumName(defaultEnumValue)
                  result   <- selectTerm(name.append(enumName))
                } yield Some(result)
              case None => Free.pure(None)
            }
          case p: StringSchema =>
            Default(p).extract[String].fold(empty)(litString(_).map(Some(_)))
          case _ =>
            Free.pure(None)
        }
    }

  }
}
