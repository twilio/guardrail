package com.twilio.guardrail.generators

import cats.~>
import cats.instances.list._
import cats.instances.option._
import cats.syntax.traverse._
import com.github.javaparser.ast._
import com.github.javaparser.ast.Modifier._
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, PrimitiveType, Type, VoidType, ArrayType => AstArrayType }
import com.github.javaparser.ast.body.{ BodyDeclaration, Parameter, TypeDeclaration }
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt.Statement
import com.twilio.guardrail._
import com.twilio.guardrail.Common.resolveFile
import com.twilio.guardrail.generators.syntax.Java._
import com.twilio.guardrail.generators.syntax.RichString
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.terms._
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.Locale

import com.github.javaparser.StaticJavaParser
import org.eclipse.jdt.core.{ JavaCore, ToolFactory }
import org.eclipse.jdt.core.formatter.{ CodeFormatter, DefaultCodeFormatterConstants }
import org.eclipse.jface.text.Document
import scala.collection.JavaConverters._
import scala.util.Try

object JavaGenerator {
  def buildPkgDecl(parts: List[String]): Target[PackageDeclaration] = safeParseName(parts.mkString(".")).map(new PackageDeclaration(_))

  def buildMethodCall(name: String, arg: Option[Node] = None): Target[Node] = arg match {
    case Some(expr: Expression) => Target.pure(new MethodCallExpr(name, expr))
    case None                   => Target.pure(new MethodCallExpr(name))
    case other                  => Target.raiseError(s"Need expression to call '${name}' but got a ${other.getClass.getName} instead")
  }

  private val formatter = ToolFactory.createCodeFormatter(
    Map(
      JavaCore.COMPILER_SOURCE                                                     -> JavaCore.VERSION_1_8,
      JavaCore.COMPILER_COMPLIANCE                                                 -> JavaCore.VERSION_1_8,
      JavaCore.COMPILER_CODEGEN_TARGET_PLATFORM                                    -> JavaCore.VERSION_1_8,
      DefaultCodeFormatterConstants.FORMATTER_TAB_CHAR                             -> JavaCore.SPACE,
      DefaultCodeFormatterConstants.FORMATTER_TAB_SIZE                             -> "4",
      DefaultCodeFormatterConstants.FORMATTER_INSERT_SPACE_BEFORE_COLON_IN_CASE    -> DefaultCodeFormatterConstants.FALSE,
      DefaultCodeFormatterConstants.FORMATTER_INSERT_SPACE_BEFORE_COLON_IN_DEFAULT -> DefaultCodeFormatterConstants.FALSE
    ).asJava
  )

  def prettyPrintSource(source: CompilationUnit): Target[Array[Byte]] = {
    val _         = source.getChildNodes.asScala.headOption.fold(source.addOrphanComment _)(_.setComment)(GENERATED_CODE_COMMENT)
    val className = Try[TypeDeclaration[_]](source.getType(0)).fold(_ => "(unknown)", _.getNameAsString)
    val sourceStr = source.toString
    Option(formatter.format(CodeFormatter.K_COMPILATION_UNIT, sourceStr, 0, sourceStr.length, 0, "\n"))
      .fold(
        Target.raiseError[Array[Byte]](s"Failed to format class '$className'")
      )({ textEdit =>
        val doc = new Document(sourceStr)
        Try(textEdit.apply(doc)).fold(
          t => Target.raiseError[Array[Byte]](s"Failed to format class '$className': $t"),
          _ => Target.pure(doc.get.getBytes(StandardCharsets.UTF_8))
        )
      })
  }

  def writeClientTree(
      pkgPath: Path,
      pkg: List[String],
      pkgDecl: PackageDeclaration,
      imports: List[ImportDeclaration],
      definition: BodyDeclaration[_ <: BodyDeclaration[_]]
  ): Target[WriteTree] =
    definition match {
      case td: TypeDeclaration[_] =>
        val cu = new CompilationUnit()
        cu.setPackageDeclaration(pkgDecl)
        imports.map(cu.addImport)
        cu.addType(td)
        prettyPrintSource(cu).map(bytes => WriteTree(resolveFile(pkgPath)(pkg :+ s"${td.getNameAsString}.java"), bytes))
      case other =>
        Target.raiseError(s"Class definition must be a TypeDeclaration but it is a ${other.getClass.getName}")
    }

  def writeServerTree(
      pkgPath: Path,
      pkg: List[String],
      pkgDecl: PackageDeclaration,
      imports: List[ImportDeclaration],
      definition: BodyDeclaration[_ <: BodyDeclaration[_]]
  ): Target[WriteTree] =
    definition match {
      case td: TypeDeclaration[_] =>
        val cu = new CompilationUnit()
        cu.setPackageDeclaration(pkgDecl)
        imports.map(cu.addImport)
        cu.addType(td)
        prettyPrintSource(cu).map(bytes => WriteTree(resolveFile(pkgPath)(pkg :+ s"${td.getNameAsString}.java"), bytes))
      case other =>
        Target.raiseError(s"Class definition must be a TypeDeclaration but it is a ${other.getClass.getName}")
    }

  object JavaInterp extends (ScalaTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ScalaTerm[JavaLanguage, T]): Target[T] = term match {
      case VendorPrefixes() => Target.pure(List("x-java", "x-jvm"))

      case LitString(value)        => Target.pure(new StringLiteralExpr(value))
      case LitFloat(value)         => Target.pure(new DoubleLiteralExpr(value))
      case LitDouble(value)        => Target.pure(new DoubleLiteralExpr(value))
      case LitInt(value)           => Target.pure(new IntegerLiteralExpr(value))
      case LitLong(value)          => Target.pure(new LongLiteralExpr(value))
      case LitBoolean(value)       => Target.pure(new BooleanLiteralExpr(value))
      case LiftOptionalType(value) => safeParseClassOrInterfaceType(s"java.util.Optional").map(_.setTypeArguments(new NodeList(value)))
      case LiftOptionalTerm(value) => buildMethodCall("java.util.Optional.ofNullable", Some(value))
      case EmptyOptionalTerm()     => buildMethodCall("java.util.Optional.empty")
      case EmptyArray() =>
        for {
          cls <- safeParseClassOrInterfaceType("java.util.ArrayList")
        } yield {
          new ObjectCreationExpr(
            null,
            cls.setTypeArguments(new NodeList[Type]),
            new NodeList()
          )
        }
      case EmptyMap() =>
        Target.pure(
          new ObjectCreationExpr(
            null,
            StaticJavaParser
              .parseClassOrInterfaceType("java.util.HashMap")
              .setTypeArguments(new NodeList[Type]),
            new NodeList()
          )
        )
      case LiftVectorType(value, customTpe) =>
        customTpe
          .fold[Target[ClassOrInterfaceType]](safeParseClassOrInterfaceType("java.util.List").map(identity))({
            case t: ClassOrInterfaceType => Target.pure(t)
            case x                       => Target.raiseError(s"Unsure how to map $x")
          })
          .map(_.setTypeArguments(new NodeList(value)))
      case LiftVectorTerm(value) => buildMethodCall("java.util.Collections.singletonList", Some(value))
      case LiftMapType(value, customTpe) =>
        customTpe
          .fold[Target[ClassOrInterfaceType]](safeParseClassOrInterfaceType("java.util.Map").map(identity))({
            case t: ClassOrInterfaceType => Target.pure(t)
            case x                       => Target.raiseError(s"Unsure how to map $x")
          })
          .map(_.setTypeArguments(STRING_TYPE, value))
      case FullyQualifyPackageName(rawPkgName) => Target.pure(rawPkgName)
      case LookupEnumDefaultValue(tpe, defaultValue, values) => {
        // FIXME: Is there a better way to do this? There's a gap of coverage here
        defaultValue match {
          case s: StringLiteralExpr =>
            values
              .find(_._1 == s.getValue)
              .fold(Target.raiseError[Name](s"Enumeration ${tpe} is not defined for default value ${s.getValue}"))(value => Target.pure(value._3))
          case _ =>
            Target.raiseError(s"Enumeration ${tpe} somehow has a default value that isn't a string")
        }
      }
      case FormatEnumName(enumValue) => Target.pure(enumValue.toSnakeCase.toUpperCase(Locale.US))
      case EmbedArray(tpe, containerTpe) =>
        tpe match {
          case SwaggerUtil.Deferred(tpe) =>
            Target.pure(SwaggerUtil.DeferredArray(tpe, containerTpe))
          case SwaggerUtil.DeferredArray(_, _) =>
            Target.raiseError("FIXME: Got an Array of Arrays, currently not supported")
          case SwaggerUtil.DeferredMap(_, _) =>
            Target.raiseError("FIXME: Got an Array of Maps, currently not supported")
        }
      case EmbedMap(tpe, containerTpe) =>
        tpe match {
          case SwaggerUtil.Deferred(inner) => Target.pure(SwaggerUtil.DeferredMap(inner, containerTpe))
          case SwaggerUtil.DeferredMap(_, _) =>
            Target.raiseError("FIXME: Got a map of maps, currently not supported")
          case SwaggerUtil.DeferredArray(_, _) =>
            Target.raiseError("FIXME: Got a map of arrays, currently not supported")
        }
      case ParseType(tpe) =>
        safeParseType(tpe)
          .map(Option.apply)
          .recover {
            case err =>
              println(s"Warning: Unparsable x-java-type: ${tpe} ${err}")
              None
          }
      case ParseTypeName(tpe) =>
        Option(tpe).map(_.trim).filterNot(_.isEmpty).traverse(safeParseName)

      case PureTermName(tpe) =>
        Option(tpe).map(_.trim).filterNot(_.isEmpty).map(_.escapeIdentifier).map(safeParseName).getOrElse(Target.raiseError("A structure's name is empty"))

      case PureTypeName(tpe) =>
        Option(tpe).map(_.trim).filterNot(_.isEmpty).map(safeParseName).getOrElse(Target.raiseError("A structure's name is empty"))

      case PureMethodParameter(nameStr, tpe, default) =>
        safeParseSimpleName(nameStr.asString.escapeIdentifier).map(name => new Parameter(new NodeList(finalModifier), tpe, name))

      case TypeNamesEqual(a, b) =>
        Target.pure(a.asString == b.asString)

      case TypesEqual(a, b) =>
        Target.pure(a.equals(b))

      case ExtractTypeName(tpe) =>
        def extractTypeName(tpe: Type): Target[Name] = tpe match {
          case a: AstArrayType if a.getComponentType.isPrimitiveType => extractTypeName(new AstArrayType(a.getComponentType.asPrimitiveType().toBoxedType))
          case a: AstArrayType                                       => safeParseName(a.asString)
          case ci: ClassOrInterfaceType                              => safeParseName(ci.getNameAsString)
          case p: PrimitiveType                                      => safeParseName(p.toBoxedType.getNameAsString)
          case _: VoidType                                           => safeParseName("Void")
          case _ =>
            println(s"WARN: ExtractTypeName: unhandled type ${tpe.getClass.getName}")
            safeParseName("Void")
        }

        extractTypeName(tpe).map(Option.apply)

      case ExtractTermName(term) =>
        Target.pure(term.asString)

      case SelectType(typeNames) =>
        safeParseType(typeNames.toList.mkString("."))

      case SelectTerm(termNames) =>
        safeParseExpression[Expression](termNames.toList.mkString(".")).map(v => v: Node)

      case AlterMethodParameterName(param, name) =>
        safeParseSimpleName(name.asString.escapeIdentifier).map(
          new Parameter(
            param.getTokenRange.orElse(null),
            param.getModifiers,
            param.getAnnotations,
            param.getType,
            param.isVarArgs,
            param.getVarArgsAnnotations,
            _
          )
        )

      case BytesType()               => Target.raiseError("format: bytes not supported for Java")
      case DateType()                => safeParseType("java.time.LocalDate")
      case DateTimeType()            => safeParseType("java.time.OffsetDateTime")
      case UUIDType()                => safeParseType("java.util.UUID")
      case StringType(format)        => format.fold(Target.pure[Type](STRING_TYPE))(safeParseType)
      case FloatType()               => safeParseType("Float")
      case DoubleType()              => safeParseType("Double")
      case NumberType(format)        => safeParseType("java.math.BigDecimal")
      case IntType()                 => safeParseType("Integer")
      case LongType()                => safeParseType("Long")
      case IntegerType(format)       => safeParseType("java.math.BigInteger")
      case BooleanType(format)       => safeParseType("Boolean")
      case ArrayType(format)         => safeParseClassOrInterfaceType("java.util.List").map(_.setTypeArguments(new NodeList[Type](STRING_TYPE)))
      case FallbackType(tpe, format) => Target.fromOption(tpe, "Missing type").flatMap(safeParseType)

      case WidenTypeName(tpe)           => safeParseType(tpe.asString)
      case WidenTermSelect(value)       => Target.pure(value)
      case WidenClassDefinition(value)  => Target.pure(value)
      case WidenObjectDefinition(value) => Target.pure(value)

      case FindCommonDefaultValue(history, a, b) =>
        (a, b) match {
          case (Some(va), Some(vb)) =>
            if (va.toString() == vb.toString()) {
              Target.pure(Some(va))
            } else {
              Target.raiseError(
                s"There is a mismatch at ${history} between default values ${va} and ${vb}. This parameter is defined at multiple places and those definitions are incompatible with each other. They must have the same name, type and default value. (${history})"
              )
            }
          case (va, vb) => Target.pure(va.orElse(vb))
        }
      case FindCommonRawType(history, a, b) =>
        if (a == b) {
          Target.pure(a)
        } else {
          Target.raiseError(
            s"There is a mismatch at ${history} between types ${a} and ${b}. Conflicting definitions between types and inherited types are not supported."
          )
        }

      case RenderImplicits(pkgPath, pkgName, frameworkImports, jsonImports, customImports) =>
        Target.pure(None)

      case RenderFrameworkImplicits(pkgPath, pkgName, frameworkImports, jsonImports, frameworkImplicits, frameworkImplicitName) =>
        Target.raiseError("Java does not support Framework Implicits")

      case RenderFrameworkDefinitions(pkgPath, pkgName, frameworkImports, frameworkDefinitions, frameworkDefinitionsName) =>
        for {
          pkgDecl <- buildPkgDecl(pkgName)
          cu = {
            val cu = new CompilationUnit()
            cu.setPackageDeclaration(pkgDecl)
            frameworkImports.map(cu.addImport)
            cu.addType(frameworkDefinitions)
            cu
          }
          bytes <- prettyPrintSource(cu)
        } yield WriteTree(resolveFile(pkgPath)(List(s"${frameworkDefinitionsName.asString}.java")), bytes)

      case WritePackageObject(dtoPackagePath, dtoComponents, customImports, packageObjectImports, protocolImports, packageObjectContents, extraTypes) =>
        for {
          pkgDecl <- dtoComponents.traverse(xs => buildPkgDecl(xs.toList))
          bytes   <- pkgDecl.traverse(x => prettyPrintSource(new CompilationUnit().setPackageDeclaration(x)))
        } yield bytes.map(WriteTree(resolveFile(dtoPackagePath)(List.empty).resolve("package-info.java"), _))

      case WriteProtocolDefinition(outputPath, pkgName, definitions, dtoComponents, imports, elem) =>
        for {
          pkgDecl      <- buildPkgDecl(dtoComponents)
          showerImport <- safeParseRawImport((pkgName :+ "Shower").mkString("."))

          nameAndCompilationUnit = elem match {
            case EnumDefinition(_, _, _, _, cls, staticDefns) =>
              val cu = new CompilationUnit()
              cu.setPackageDeclaration(pkgDecl)
              imports.foreach(cu.addImport)
              staticDefns.extraImports.foreach(cu.addImport)
              cu.addImport(showerImport)
              val clsCopy = cls.clone()
              staticDefns.definitions.foreach(clsCopy.addMember)
              cu.addType(clsCopy)
              Option((cls.getName.getIdentifier, cu))

            case ClassDefinition(_, _, _, cls, staticDefns, _) =>
              val cu = new CompilationUnit()
              cu.setPackageDeclaration(pkgDecl)
              imports.foreach(cu.addImport)
              staticDefns.extraImports.foreach(cu.addImport)
              cu.addImport(showerImport)
              val clsCopy = cls.clone()
              staticDefns.definitions.foreach(clsCopy.addMember)
              cu.addType(clsCopy)
              Option((cls.getName.getIdentifier, cu))

            case ADT(name, tpe, _, trt, staticDefns) =>
              val cu = new CompilationUnit()
              cu.setPackageDeclaration(pkgDecl)
              imports.foreach(cu.addImport)
              staticDefns.extraImports.foreach(cu.addImport)
              val trtCopy = trt.clone()
              staticDefns.definitions.foreach(trtCopy.addMember)
              cu.addType(trtCopy)
              Option((name, cu))

            case RandomType(_, _) =>
              Option.empty
          }

          nameAndBytes <- nameAndCompilationUnit.fold(Target.pure(Option.empty[(String, Array[Byte])]))({
            case (name, cu) =>
              prettyPrintSource(cu).map(bytes => Option((name, bytes)))
          })
        } yield nameAndBytes.fold((List.empty[WriteTree], List.empty[Statement]))({
          case (name, bytes) =>
            (
              List(WriteTree(resolveFile(outputPath)(dtoComponents).resolve(s"${name}.java"), bytes)),
              List.empty[Statement]
            )
        })

      case WriteClient(
          pkgPath,
          pkgName,
          customImports,
          frameworkImplicitName,
          dtoComponents,
          Client(pkg, clientName, imports, staticDefns, client, responseDefinitions)
          ) =>
        for {
          pkgDecl             <- buildPkgDecl(pkgName ++ pkg)
          commonImport        <- safeParseRawImport((pkgName :+ "*").mkString("."))
          dtoComponentsImport <- dtoComponents.traverse(x => safeParseRawImport((x :+ "*").mkString(".")))
          allImports    = imports ++ (customImports :+ commonImport) ++ dtoComponentsImport
          clientClasses = client.map(_.merge).toList
          trees <- (clientClasses ++ responseDefinitions).traverse(writeClientTree(pkgPath, pkg, pkgDecl, allImports, _))
        } yield trees

      case WriteServer(
          pkgPath,
          pkgName,
          customImports,
          frameworkImplicitName,
          dtoComponents,
          Server(pkg, extraImports, handlerDefinition, serverDefinitions)
          ) =>
        for {
          pkgDecl <- buildPkgDecl(pkgName ++ pkg)

          commonImport        <- safeParseRawImport((pkgName :+ "*").mkString("."))
          dtoComponentsImport <- dtoComponents.traverse(x => safeParseRawImport((x :+ "*").mkString(".")))
          allImports = extraImports ++ List(commonImport) ++ dtoComponentsImport ++ customImports

          handlerTree <- writeServerTree(pkgPath, pkg, pkgDecl, allImports, handlerDefinition)
          serverTrees <- serverDefinitions.traverse(writeServerTree(pkgPath, pkg, pkgDecl, allImports, _))
        } yield handlerTree +: serverTrees

      case WrapToObject(_, _, _) =>
        Target.pure(Option.empty) // Unnecessary in Java, due to static members
    }
  }
}
