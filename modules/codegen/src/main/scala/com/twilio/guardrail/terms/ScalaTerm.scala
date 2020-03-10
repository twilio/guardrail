package com.twilio.guardrail
package terms

import com.twilio.guardrail.SwaggerUtil.LazyResolvedType
import com.twilio.guardrail.generators.RawParameterType
import com.twilio.guardrail.languages.LA
import java.nio.file.Path

import cats.data.NonEmptyList

sealed trait ScalaTerm[L <: LA, T]

case class VendorPrefixes[L <: LA]() extends ScalaTerm[L, List[String]]

case class LitString[L <: LA](value: String)                                 extends ScalaTerm[L, L#Term]
case class LitFloat[L <: LA](value: Float)                                   extends ScalaTerm[L, L#Term]
case class LitDouble[L <: LA](value: Double)                                 extends ScalaTerm[L, L#Term]
case class LitInt[L <: LA](value: Int)                                       extends ScalaTerm[L, L#Term]
case class LitLong[L <: LA](value: Long)                                     extends ScalaTerm[L, L#Term]
case class LitBoolean[L <: LA](value: Boolean)                               extends ScalaTerm[L, L#Term]
case class LiftOptionalType[L <: LA](value: L#Type)                          extends ScalaTerm[L, L#Type]
case class LiftOptionalTerm[L <: LA](value: L#Term)                          extends ScalaTerm[L, L#Term]
case class EmptyArray[L <: LA]()                                             extends ScalaTerm[L, L#Term]
case class EmptyMap[L <: LA]()                                               extends ScalaTerm[L, L#Term]
case class EmptyOptionalTerm[L <: LA]()                                      extends ScalaTerm[L, L#Term]
case class LiftVectorType[L <: LA](value: L#Type, customTpe: Option[L#Type]) extends ScalaTerm[L, L#Type]
case class LiftVectorTerm[L <: LA](value: L#Term)                            extends ScalaTerm[L, L#Term]
case class LiftMapType[L <: LA](value: L#Type, customTpe: Option[L#Type])    extends ScalaTerm[L, L#Type]

case class FullyQualifyPackageName[L <: LA](rawPkgName: List[String]) extends ScalaTerm[L, List[String]]

case class LookupEnumDefaultValue[L <: LA](tpe: L#TypeName, defaultValue: L#Term, values: List[(String, L#TermName, L#TermSelect)])
    extends ScalaTerm[L, L#TermSelect]
case class FormatEnumName[L <: LA](enumValue: String) extends ScalaTerm[L, String]

case class EmbedArray[L <: LA](tpe: LazyResolvedType[L], customTpe: Option[L#Type]) extends ScalaTerm[L, LazyResolvedType[L]]
case class EmbedMap[L <: LA](tpe: LazyResolvedType[L], customTpe: Option[L#Type])   extends ScalaTerm[L, LazyResolvedType[L]]

case class ParseType[L <: LA](value: String)     extends ScalaTerm[L, Option[L#Type]]
case class ParseTypeName[L <: LA](value: String) extends ScalaTerm[L, Option[L#TypeName]]
case class PureTypeName[L <: LA](value: String)  extends ScalaTerm[L, L#TypeName]
case class PureTermName[L <: LA](value: String)  extends ScalaTerm[L, L#TermName]

case class PureMethodParameter[L <: LA](name: L#TermName, tpe: L#Type, default: Option[L#Term]) extends ScalaTerm[L, L#MethodParameter]
case class TypeNamesEqual[L <: LA](a: L#TypeName, b: L#TypeName)                                extends ScalaTerm[L, Boolean]
case class TypesEqual[L <: LA](a: L#Type, b: L#Type)                                            extends ScalaTerm[L, Boolean]
case class ExtractTypeName[L <: LA](tpe: L#Type)                                                extends ScalaTerm[L, Option[L#TypeName]]
case class ExtractTermName[L <: LA](term: L#TermName)                                           extends ScalaTerm[L, String]
case class SelectType[L <: LA](typeNames: NonEmptyList[String])                                 extends ScalaTerm[L, L#Type]
case class SelectTerm[L <: LA](termNames: NonEmptyList[String])                                 extends ScalaTerm[L, L#Term]
case class AlterMethodParameterName[L <: LA](param: L#MethodParameter, name: L#TermName)        extends ScalaTerm[L, L#MethodParameter]

case class BytesType[L <: LA]()                                               extends ScalaTerm[L, L#Type]
case class UUIDType[L <: LA]()                                                extends ScalaTerm[L, L#Type]
case class DateType[L <: LA]()                                                extends ScalaTerm[L, L#Type]
case class DateTimeType[L <: LA]()                                            extends ScalaTerm[L, L#Type]
case class StringType[L <: LA](format: Option[String])                        extends ScalaTerm[L, L#Type]
case class FloatType[L <: LA]()                                               extends ScalaTerm[L, L#Type]
case class DoubleType[L <: LA]()                                              extends ScalaTerm[L, L#Type]
case class NumberType[L <: LA](format: Option[String])                        extends ScalaTerm[L, L#Type]
case class IntType[L <: LA]()                                                 extends ScalaTerm[L, L#Type]
case class LongType[L <: LA]()                                                extends ScalaTerm[L, L#Type]
case class IntegerType[L <: LA](format: Option[String])                       extends ScalaTerm[L, L#Type]
case class BooleanType[L <: LA](format: Option[String])                       extends ScalaTerm[L, L#Type]
case class ArrayType[L <: LA](format: Option[String])                         extends ScalaTerm[L, L#Type]
case class FallbackType[L <: LA](tpe: Option[String], format: Option[String]) extends ScalaTerm[L, L#Type]

case class WidenTypeName[L <: LA](tpe: L#TypeName)                   extends ScalaTerm[L, L#Type]
case class WidenTermSelect[L <: LA](value: L#TermSelect)             extends ScalaTerm[L, L#Term]
case class WidenClassDefinition[L <: LA](value: L#ClassDefinition)   extends ScalaTerm[L, L#Definition]
case class WidenObjectDefinition[L <: LA](value: L#ObjectDefinition) extends ScalaTerm[L, L#Definition]

case class FindCommonDefaultValue[L <: LA](history: String, a: Option[L#Term], b: Option[L#Term]) extends ScalaTerm[L, Option[L#Term]]
case class FindCommonRawType[L <: LA](history: String, a: RawParameterType, b: RawParameterType)  extends ScalaTerm[L, RawParameterType]

case class RenderImplicits[L <: LA](
    pkgPath: Path,
    pkgName: List[String],
    frameworkImports: List[L#Import],
    jsonImports: List[L#Import],
    customImports: List[L#Import]
) extends ScalaTerm[L, Option[WriteTree]]
case class RenderFrameworkImplicits[L <: LA](
    pkgPath: Path,
    pkgName: List[String],
    frameworkImports: List[L#Import],
    jsonImports: List[L#Import],
    frameworkImplicits: L#ObjectDefinition,
    frameworkImplicitName: L#TermName
) extends ScalaTerm[L, WriteTree]
case class RenderFrameworkDefinitions[L <: LA](
    pkgPath: Path,
    pkgName: List[String],
    frameworkImports: List[L#Import],
    frameworkDefinitions: L#ClassDefinition,
    frameworkDefinitionsName: L#TermName
) extends ScalaTerm[L, WriteTree]
case class WritePackageObject[L <: LA](
    dtoPackagePath: Path,
    dtoComponents: Option[NonEmptyList[String]],
    customImports: List[L#Import],
    packageObjectImports: List[L#Import],
    protocolImports: List[L#Import],
    packageObjectContents: List[L#ValueDefinition],
    extraTypes: List[L#Statement]
) extends ScalaTerm[L, Option[WriteTree]]
case class WriteProtocolDefinition[L <: LA](
    outputPath: Path,
    pkgName: List[String],
    definitions: List[String],
    dtoComponents: List[String],
    imports: List[L#Import],
    elem: StrictProtocolElems[L]
) extends ScalaTerm[L, (List[WriteTree], List[L#Statement])]
case class WriteClient[L <: LA](
    pkgPath: Path,
    pkgName: List[String],
    customImports: List[L#Import],
    frameworkImplicitName: Option[L#TermName],
    dtoComponents: Option[List[String]],
    client: Client[L]
) extends ScalaTerm[L, List[WriteTree]]
case class WriteServer[L <: LA](
    pkgPath: Path,
    pkgName: List[String],
    customImports: List[L#Import],
    frameworkImplicitName: Option[L#TermName],
    dtoComponents: Option[List[String]],
    server: Server[L]
) extends ScalaTerm[L, List[WriteTree]]

case class WrapToObject[L <: LA](name: L#TermName, imports: List[L#Import], definitions: List[L#Definition]) extends ScalaTerm[L, Option[L#ObjectDefinition]]
