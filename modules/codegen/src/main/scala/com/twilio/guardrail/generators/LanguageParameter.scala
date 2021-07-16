package com.twilio.guardrail
package generators

import cats.syntax.all._
import com.twilio.guardrail.SwaggerUtil.ResolvedType
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.extract.{ Default, FileHashAlgorithm }
import com.twilio.guardrail.generators.syntax._
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.shims._
import com.twilio.guardrail.terms.framework.FrameworkTerms
import com.twilio.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, SwaggerTerms }
import io.swagger.v3.oas.models.media.Schema
import io.swagger.v3.oas.models.parameters._

case class RawParameterName private[generators] (value: String)
case class RawParameterType private[generators] (tpe: Option[String], format: Option[String])
class LanguageParameters[L <: LA](val parameters: List[LanguageParameter[L]]) {
  val filterParamBy: String => List[LanguageParameter[L]] = LanguageParameter.filterParams(parameters)
  val headerParams: List[LanguageParameter[L]]            = filterParamBy("header")
  val pathParams: List[LanguageParameter[L]]              = filterParamBy("path")
  val queryStringParams: List[LanguageParameter[L]]       = filterParamBy("query")
  val bodyParams: Option[LanguageParameter[L]]            = filterParamBy("body").headOption
  val formParams: List[LanguageParameter[L]]              = filterParamBy("formData")
}
class LanguageParameter[L <: LA] private[generators] (
    val in: Option[String],
    val param: L#MethodParameter,
    val paramName: L#TermName,
    val argName: RawParameterName,
    val argType: L#Type,
    val rawType: RawParameterType,
    val required: Boolean,
    val hashAlgorithm: Option[String],
    val isFile: Boolean
) {
  override def toString: String =
    s"LanguageParameter($in, $param, $paramName, $argName, $argType)"

  def withType(newArgType: L#Type, rawType: Option[String] = this.rawType.tpe, rawFormat: Option[String] = this.rawType.format): LanguageParameter[L] =
    new LanguageParameter[L](in, param, paramName, argName, newArgType, RawParameterType(rawType, rawFormat), required, hashAlgorithm, isFile)

  def withParamName(newParamName: L#TermName): LanguageParameter[L] =
    new LanguageParameter[L](in, param, newParamName, argName, argType, rawType, required, hashAlgorithm, isFile)
}
object LanguageParameter {
  def unapply[L <: LA](param: LanguageParameter[L]): Option[(Option[String], L#MethodParameter, L#TermName, RawParameterName, L#Type)] =
    Some((param.in, param.param, param.paramName, param.argName, param.argType))

  def fromParameter[L <: LA, F[_]](
      protocolElems: List[StrictProtocolElems[L]]
  )(
      implicit Fw: FrameworkTerms[L, F],
      Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Tracker[Parameter] => F[LanguageParameter[L]] = { parameter =>
    import Cl._
    import Fw._
    import Sc._
    import Sw._

    def paramMeta(param: Tracker[Parameter]): F[SwaggerUtil.ResolvedType[L]] = {
      def getDefault[U <: Parameter: Default.GetDefault](_type: String, fmt: Tracker[Option[String]], p: Tracker[U]): F[Option[L#Term]] =
        (_type, fmt.unwrapTracker) match {
          case ("string", None) =>
            Default(p).extract[String].traverse(litString(_))
          case ("number", Some("float")) =>
            Default(p).extract[Float].traverse(litFloat(_))
          case ("number", Some("double")) =>
            Default(p).extract[Double].traverse(litDouble(_))
          case ("integer", Some("int32")) =>
            Default(p).extract[Int].traverse(litInt(_))
          case ("integer", Some("int64")) =>
            Default(p).extract[Long].traverse(litLong(_))
          case ("boolean", None) =>
            Default(p).extract[Boolean].traverse(litBoolean(_))
          case x => Option.empty[L#Term].pure[F]
        }

      def resolveParam(param: Tracker[Parameter], typeFetcher: Tracker[Parameter] => F[Tracker[String]]): F[ResolvedType[L]] =
        for {
          tpeName <- typeFetcher(param)
          schema = param.downField("schema", _.getSchema)
          fmt    = schema.flatDownField("format", _.getFormat)
          customParamTypeName  <- SwaggerUtil.customTypeName(param)
          customSchemaTypeName <- schema.unwrapTracker.flatTraverse(SwaggerUtil.customTypeName(_: Schema[_]))
          customTypeName = Tracker.cloneHistory(schema, customSchemaTypeName).fold(Tracker.cloneHistory(param, customParamTypeName))(_.map(Option.apply))
          tpe <- SwaggerUtil.typeName[L, F](tpeName.map(Option(_)), fmt, customTypeName)
          resolvedType <- schema.fold[F[ResolvedType[L]]](
            (tpe.pure[F], getDefault(tpeName.unwrapTracker, fmt, param))
              .mapN(SwaggerUtil.Resolved[L](_, None, _, Some(tpeName.unwrapTracker), fmt.unwrapTracker))
          )(schema => SwaggerUtil.propMetaWithName(tpe, schema, liftArrayType))
        } yield resolvedType

      def paramHasRefSchema(p: Parameter): Boolean = Option(p.getSchema).exists(s => Option(s.get$ref()).nonEmpty)

      param
        .refine[F[SwaggerUtil.ResolvedType[L]]]({ case r: Parameter if r.isRef => r })(
          r => getRefParameterRef(r).map(_.unwrapTracker).map(SwaggerUtil.Deferred(_))
        )
        .orRefine({ case r: Parameter if paramHasRefSchema(r) => r })(r => getSimpleRef(r.downField("schema", _.getSchema)).map(SwaggerUtil.Deferred(_)))
        .orRefine({ case x: Parameter if x.isInBody => x })(
          x =>
            getBodyParameterSchema(x)
              .flatMap(x => SwaggerUtil.modelMetaType[L, F](x))
        )
        .orRefine({ case x: Parameter if x.isInHeader => x })(x => resolveParam(x, getHeaderParameterType))
        .orRefine({ case x: Parameter if x.isInPath => x })(x => resolveParam(x, getPathParameterType))
        .orRefine({ case x: Parameter if x.isInQuery => x })(x => resolveParam(x, getQueryParameterType))
        .orRefine({ case x: Parameter if x.isInCookies => x })(x => resolveParam(x, getCookieParameterType))
        .orRefine({ case x: Parameter if x.isInFormData => x })(x => resolveParam(x, getFormParameterType))
        .orRefineFallback(fallbackParameterHandler(_))
    }

    log.function(s"fromParameter")(for {
      _                                                                        <- log.debug(parameter.unwrapTracker.showNotNull)
      meta                                                                     <- paramMeta(parameter)
      SwaggerUtil.Resolved(paramType, _, baseDefaultValue, rawType, rawFormat) <- SwaggerUtil.ResolvedType.resolve[L, F](meta, protocolElems)

      required = parameter.downField("required", _.getRequired()).map(_.getOrElse(false)).unwrapTracker
      declType <- if (!required) {
        liftOptionalType(paramType)
      } else {
        paramType.pure[F]
      }

      enumDefaultValue <- extractTypeName(paramType).flatMap(_.fold(baseDefaultValue.traverse(_.pure[F])) { tpe =>
        protocolElems
          .flatTraverse({
            case x @ EnumDefinition(_, _tpeName, _, _, _, _) =>
              for {
                areEqual <- typeNamesEqual(tpe, _tpeName)
              } yield if (areEqual) List(x) else List.empty[EnumDefinition[L]]
            case _ => List.empty[EnumDefinition[L]].pure[F]
          })
          .flatMap(_.headOption.fold[F[Option[L#Term]]](baseDefaultValue.traverse(_.pure[F])) { x =>
            baseDefaultValue.traverse(lookupEnumDefaultValue(tpe, _, x.elems).flatMap(widenTermSelect))
          })
      })

      defaultValue <- if (!required) {
        (enumDefaultValue.traverse(liftOptionalTerm), emptyOptionalTerm().map(Option.apply _)).mapN(_.orElse(_))
      } else {
        enumDefaultValue.pure[F]
      }

      name <- getParameterName(parameter)

      paramName     <- formatMethodArgName(name)
      paramTermName <- pureTermName(paramName)
      param         <- pureMethodParameter(paramTermName, declType, defaultValue)

      ftpe       <- fileType(None)
      isFileType <- typesEqual(paramType, ftpe)
    } yield {
      new LanguageParameter[L](
        parameter.downField("in", _.getIn()).unwrapTracker,
        param,
        paramTermName,
        RawParameterName(name),
        declType,
        RawParameterType(rawType, rawFormat),
        required,
        FileHashAlgorithm(parameter),
        isFileType
      )
    })
  }

  def fromParameters[L <: LA, F[_]](
      protocolElems: List[StrictProtocolElems[L]]
  )(
      implicit Fw: FrameworkTerms[L, F],
      Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): List[Tracker[Parameter]] => F[List[LanguageParameter[L]]] = { params =>
    import Sc._
    for {
      parameters <- params.traverse(fromParameter(protocolElems))
      counts     <- parameters.traverse(param => extractTermName(param.paramName)).map(_.groupBy(identity).mapValues(_.length))
      result <- parameters.traverse { param =>
        extractTermName(param.paramName).flatMap { name =>
          if (counts.getOrElse(name, 0) > 1) {
            pureTermName(param.argName.value).flatMap { escapedName =>
              alterMethodParameterName(param.param, escapedName).map { newParam =>
                new LanguageParameter[L](
                  param.in,
                  newParam,
                  escapedName,
                  param.argName,
                  param.argType,
                  param.rawType,
                  param.required,
                  param.hashAlgorithm,
                  param.isFile
                )
              }
            }
          } else param.pure[F]
        }
      }
    } yield result
  }

  /**
    * Create method parameters from Swagger's Path parameters list. Use Option for non-required parameters.
    * @param params
    * @return
    */
  def filterParams[L <: LA](params: List[LanguageParameter[L]]): String => List[LanguageParameter[L]] = { in =>
    params.filter(_.in == Some(in))
  }
}
