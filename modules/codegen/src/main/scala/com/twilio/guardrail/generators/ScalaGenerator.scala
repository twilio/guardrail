package com.twilio.guardrail.generators

import cats.Monad
import cats.data.NonEmptyList
import cats.implicits._
import com.twilio.guardrail.Common.resolveFile
import com.twilio.guardrail.SwaggerUtil.LazyResolvedType
import com.twilio.guardrail._
import com.twilio.guardrail.generators.syntax.RichString
import com.twilio.guardrail.generators.syntax.Scala._
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.terms._
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import scala.meta._

object ScalaGenerator {
  private def sourceToBytes(destPath: Path, source: Source): Future[Array[Byte]] = Future {
    (formatSource(destPath, GENERATED_CODE_COMMENT + source.syntax)).getBytes(StandardCharsets.UTF_8)
  }

  private lazy val scalafmtConfPath: Path = {
    import java.nio.file.Files
    import scala.io.Source
    import scala.util.Try
    val confStream = getClass.getClassLoader.getResourceAsStream("scalafmt.conf")
    val confFile   = Files.createTempFile("scalafmt.", ".conf")
    sys.addShutdownHook(Try(Files.delete(confFile)))
    val source = Source.fromInputStream(confStream, "UTF-8")
    Files.write(confFile, source.mkString.getBytes(StandardCharsets.UTF_8))
    source.close()
    confFile
  }

  private def formatSource(destPath: Path, source: String): String = {
    import org.scalafmt.interfaces.Scalafmt
    val scalafmt = Scalafmt.create(getClass.getClassLoader)
    scalafmt.format(scalafmtConfPath, destPath, source)
  }

  object ScalaInterp extends LanguageTerms[ScalaLanguage, Target] {
    // TODO: Very interesting bug. 2.11.12 barfs if these two definitions are
    // defined inside `apply`. Once 2.11 is dropped, these can be moved back.
    val matchImplicit: PartialFunction[Stat, Defn.Val] = {
      case x: Defn.Val if (x match { case q"implicit val $_: $_ = $_" => true; case _ => false }) => x
    }
    val partitionImplicits: PartialFunction[Stat, Boolean] = matchImplicit.andThen(_ => true).orElse({ case _ => false })

    val buildPkgTerm: List[String] => Term.Ref =
      _.map(Term.Name.apply _).reduceLeft(Term.Select.apply _)

    implicit def MonadF: Monad[Target]         = Target.targetInstances
    def vendorPrefixes(): Target[List[String]] = Target.pure(List("x-scala", "x-jvm"))

    def litString(value: String): Target[scala.meta.Term]                 = Target.pure(Lit.String(value))
    def litFloat(value: Float): Target[scala.meta.Term]                   = Target.pure(Lit.Float(value))
    def litDouble(value: Double): Target[scala.meta.Term]                 = Target.pure(Lit.Double(value))
    def litInt(value: Int): Target[scala.meta.Term]                       = Target.pure(Lit.Int(value))
    def litLong(value: Long): Target[scala.meta.Term]                     = Target.pure(Lit.Long(value))
    def litBoolean(value: Boolean): Target[scala.meta.Term]               = Target.pure(Lit.Boolean(value))
    def liftOptionalType(value: scala.meta.Type): Target[scala.meta.Type] = Target.pure(t"Option[$value]")
    def liftOptionalTerm(value: scala.meta.Term): Target[scala.meta.Term] = Target.pure(q"Option($value)")
    def emptyArray(): Target[scala.meta.Term]                             = Target.pure(q"Vector.empty")
    def emptyMap(): Target[scala.meta.Term]                               = Target.pure(q"Map.empty")
    def emptyOptionalTerm(): Target[scala.meta.Term]                      = Target.pure(q"None")
    def liftVectorType(value: scala.meta.Type, customTpe: Option[scala.meta.Type]): Target[scala.meta.Type] =
      Target.pure(t"${customTpe.getOrElse(t"Vector")}[$value]")
    def liftVectorTerm(value: scala.meta.Term): Target[scala.meta.Term] = Target.pure(q"Vector($value)")
    def liftMapType(value: scala.meta.Type, customTpe: Option[scala.meta.Type]): Target[scala.meta.Type] =
      Target.pure(t"${customTpe.getOrElse(t"Map")}[String, $value]")

    def fullyQualifyPackageName(rawPkgName: List[String]): Target[List[String]] = Target.pure("_root_" +: rawPkgName)

    def lookupEnumDefaultValue(
        tpe: scala.meta.Type.Name,
        defaultValue: scala.meta.Term,
        values: List[(String, scala.meta.Term.Name, scala.meta.Term.Select)]
    ): Target[scala.meta.Term.Select] =
      defaultValue match {
        case Lit.String(name) =>
          values
            .find(_._1 == name)
            .fold(Target.raiseUserError[Term.Select](s"Enumeration $tpe is not defined for default value $name"))(value => Target.pure(value._3))
        case _ =>
          Target.raiseUserError[Term.Select](s"Enumeration $tpe somehow has a default value that isn't a string")
      }

    def formatPackageName(packageName: List[String]): Target[List[String]]              = Target.pure(packageName.map(_.toCamelCase))
    def formatTypeName(typeName: String, suffix: Option[String] = None): Target[String] = Target.pure(typeName.toPascalCase + suffix.fold("")(_.toPascalCase))
    def formatFieldName(fieldName: String): Target[String]                              = Target.pure(fieldName.toCamelCase)
    def formatMethodName(methodName: String): Target[String]                            = Target.pure(methodName.toCamelCase)
    def formatMethodArgName(methodArgName: String): Target[String]                      = Target.pure(methodArgName.toCamelCase)
    def formatEnumName(enumValue: String): Target[String]                               = Target.pure(enumValue.toPascalCase)

    def embedArray(tpe: LazyResolvedType[ScalaLanguage], containerTpe: Option[scala.meta.Type]): Target[LazyResolvedType[ScalaLanguage]] = tpe match {
      case SwaggerUtil.Deferred(tpe) =>
        Target.pure(SwaggerUtil.DeferredArray[ScalaLanguage](tpe, containerTpe))
      case SwaggerUtil.DeferredArray(_, _) =>
        Target.raiseUserError("FIXME: Got an Array of Arrays, currently not supported")
      case SwaggerUtil.DeferredMap(_, _) =>
        Target.raiseUserError("FIXME: Got an Array of Maps, currently not supported")
    }
    def embedMap(tpe: LazyResolvedType[ScalaLanguage], containerTpe: Option[scala.meta.Type]): Target[LazyResolvedType[ScalaLanguage]] = tpe match {
      case SwaggerUtil.Deferred(inner) =>
        Target.pure(SwaggerUtil.DeferredMap[ScalaLanguage](inner, containerTpe))
      case SwaggerUtil.DeferredMap(_, _) =>
        Target.raiseUserError("FIXME: Got a map of maps, currently not supported")
      case SwaggerUtil.DeferredArray(_, _) =>
        Target.raiseUserError("FIXME: Got a map of arrays, currently not supported")
    }

    def parseType(tpe: String): Target[Option[scala.meta.Type]] =
      Target.pure(
        tpe
          .parse[Type]
          .fold({ err =>
            println(s"Warning: Unparsable x-scala-type: $tpe $err")
            None
          }, Option.apply _)
      )
    def parseTypeName(tpe: String): Target[Option[scala.meta.Type.Name]] = Target.pure(Option(tpe.trim).filterNot(_.isEmpty).map(Type.Name(_)))
    def pureTermName(tpe: String): Target[scala.meta.Term.Name] =
      Target.fromOption(Option(tpe.trim).filterNot(_.isEmpty).map(Term.Name(_)), UserError("A structure's name is empty"))
    def pureTypeName(tpe: String): Target[scala.meta.Type.Name] =
      Target.fromOption(Option(tpe.trim).filterNot(_.isEmpty).map(Type.Name(_)), UserError("A structure's name is empty"))

    def pureMethodParameter(name: scala.meta.Term.Name, tpe: scala.meta.Type, default: Option[scala.meta.Term]): Target[scala.meta.Term.Param] =
      Target.pure(param"$name: $tpe".copy(default = default))
    def typeNamesEqual(a: scala.meta.Type.Name, b: scala.meta.Type.Name): Target[Boolean] = Target.pure(a.value == b.value)
    def typesEqual(a: scala.meta.Type, b: scala.meta.Type): Target[Boolean]               = Target.pure(a.structure == b.structure)
    def extractTypeName(tpe: scala.meta.Type): Target[Option[scala.meta.Type.Name]] =
      Target.pure(tpe match {
        case x: Type.Name =>
          Option(x)
        case _ =>
          Option.empty
      })
    def extractTermName(term: scala.meta.Term.Name): Target[String] = {
      val Term.Name(name) = term
      Target.pure(name)
    }
    def extractTermNameFromParam(param: scala.meta.Term.Param): Target[String] = Target.pure(param.name.value)
    def selectType(typeNames: NonEmptyList[String]): Target[scala.meta.Type] = {
      val tpe   = Type.Name(typeNames.last)
      val names = typeNames.init.map(Term.Name.apply _)
      val result = names match {
        case Nil =>
          tpe
        case x :: xs =>
          val term = xs.foldLeft[Term.Ref](x)(Term.Select.apply _)
          Type.Select(term, tpe)
      }
      Target.pure(result)
    }
    def selectTerm(termNames: NonEmptyList[String]): Target[scala.meta.Term] = {
      val result = termNames.tail.foldLeft[Term](Term.Name(termNames.head))({
        case (current, next) =>
          Term.Select(current, Term.Name(next))
      })
      Target.pure(result)
    }
    def alterMethodParameterName(param: scala.meta.Term.Param, name: scala.meta.Term.Name): Target[scala.meta.Term.Param] = Target.pure(param.copy(name = name))

    def bytesType(): Target[scala.meta.Type]                                               = Target.pure(t"Base64String")
    def uuidType(): Target[scala.meta.Type]                                                = Target.pure(t"java.util.UUID")
    def dateType(): Target[scala.meta.Type]                                                = Target.pure(t"java.time.LocalDate")
    def dateTimeType(): Target[scala.meta.Type]                                            = Target.pure(t"java.time.OffsetDateTime")
    def stringType(format: Option[String]): Target[scala.meta.Type]                        = Target.pure(format.fold(t"String")(Type.Name(_)))
    def floatType(): Target[scala.meta.Type]                                               = Target.pure(t"Float")
    def doubleType(): Target[scala.meta.Type]                                              = Target.pure(t"Double")
    def numberType(format: Option[String]): Target[scala.meta.Type]                        = Target.pure(t"BigDecimal")
    def intType(): Target[scala.meta.Type]                                                 = Target.pure(t"Int")
    def longType(): Target[scala.meta.Type]                                                = Target.pure(t"Long")
    def integerType(format: Option[String]): Target[scala.meta.Type]                       = Target.pure(t"BigInt")
    def booleanType(format: Option[String]): Target[scala.meta.Type]                       = Target.pure(t"Boolean")
    def arrayType(format: Option[String]): Target[scala.meta.Type]                         = Target.pure(t"Iterable[String]")
    def fallbackType(tpe: Option[String], format: Option[String]): Target[scala.meta.Type] = Target.fromOption(tpe, UserError("Missing type")).map(Type.Name(_))

    def widenTypeName(tpe: scala.meta.Type.Name): Target[scala.meta.Type]             = Target.pure(tpe)
    def widenTermSelect(value: scala.meta.Term.Select): Target[scala.meta.Term]       = Target.pure(value)
    def widenClassDefinition(value: scala.meta.Defn.Class): Target[scala.meta.Defn]   = Target.pure(value)
    def widenObjectDefinition(value: scala.meta.Defn.Object): Target[scala.meta.Defn] = Target.pure(value)

    def findCommonDefaultValue(history: String, a: Option[scala.meta.Term], b: Option[scala.meta.Term]): Target[Option[scala.meta.Term]] = (a, b) match {
      case (Some(va), Some(vb)) =>
        if (va.structure == vb.structure) {
          Target.pure(Some(va))
        } else {
          Target.raiseUserError(
            s"There is a mismatch at $history between default values $va and $vb. This parameter is defined at multiple places and those definitions are incompatible with each other. They must have the same name, type and default value. ($history)"
          )
        }
      case (va, vb) =>
        Target.pure(va.orElse(vb))
    }
    def findCommonRawType(history: String, a: RawParameterType, b: RawParameterType): Target[RawParameterType] =
      if (a == b) {
        Target.pure(a)
      } else {
        Target.raiseUserError(
          s"There is a mismatch at $history between types $a and $b. Conflicting definitions between types and inherited types are not supported."
        )
      }

    def renderImplicits(
        pkgPath: Path,
        pkgName: List[String],
        frameworkImports: List[scala.meta.Import],
        jsonImports: List[scala.meta.Import],
        customImports: List[scala.meta.Import]
    ): Target[Option[WriteTree]] = {
      val pkg: Term.Ref = pkgName.map(Term.Name.apply _).reduceLeft(Term.Select.apply _)
      val implicits     = source"""
            package $pkg

            ..$jsonImports

            import cats.implicits._
            import cats.data.EitherT

            object Implicits {
              abstract class AddArg[T] {
                def addArg(key: String, v: T): String
              }

              object AddArg {
                def build[T](f: String => T => String): AddArg[T] = new AddArg[T] {
                  def addArg(key: String, v: T): String = f(key)(v)
                }

                implicit def addArgSeq[T](implicit ev: AddArg[T]): AddArg[Seq[T]] = build[Seq[T]](key => vs => vs.map(v => ev.addArg(key, v)).mkString("&"))
                implicit def addArgIndexedSeq[T](implicit ev: AddArg[T]): AddArg[IndexedSeq[T]] = build[IndexedSeq[T]](key => vs => vs.map(v => ev.addArg(key, v)).mkString("&"))
                implicit def addArgIterable[T](implicit ev: AddArg[T]): AddArg[Iterable[T]] = build[Iterable[T]](key => vs => vs.map(v => ev.addArg(key, v)).mkString("&"))
                implicit def addArgList[T](implicit ev: AddArg[T]): AddArg[List[T]] = build[List[T]](key => vs => vs.map(v => ev.addArg(key, v)).mkString("&"))
                implicit def addArgVector[T](implicit ev: AddArg[T]): AddArg[Vector[T]] = build[Vector[T]](key => vs => vs.map(v => ev.addArg(key, v)).mkString("&"))
                implicit def addArgOption[T](implicit ev: AddArg[T]): AddArg[Option[T]] = build[Option[T]](key => v => v.map(ev.addArg(key, _)).getOrElse(""))
              }

              abstract class AddPath[T] {
                def addPath(v: T): String
              }

              object AddPath {
                def build[T](f: T => String): AddPath[T] = new AddPath[T] {
                  def addPath(v: T): String = f(v)
                }
              }

              abstract class Show[T] {
                def show(v: T): String
              }

              object Show {
                def build[T](f: T => String): Show[T] = new Show[T] {
                  def show(v: T): String = f(v)
                }

                implicit val showString = build[String](Predef.identity)
                implicit val showInt = build[Int](_.toString)
                implicit val showLong = build[Long](_.toString)
                implicit val showFloat = build[Float](_.toString)
                implicit val showDouble = build[Double](_.toString)
                implicit val showBigInt = build[BigInt](_.toString)
                implicit val showBigDecimal = build[BigDecimal](_.toString)
                implicit val showBoolean = build[Boolean](_.toString)
                implicit val showLocalDate = build[java.time.LocalDate](_.format(java.time.format.DateTimeFormatter.ISO_DATE))
                implicit val showOffsetDateTime = build[java.time.OffsetDateTime](_.format(java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME))
                implicit val showJavaURL = build[java.net.URI](_.toString)
                implicit val showUUID = build[java.util.UUID](_.toString)
              }

              object Formatter {
                def show[T](value: T)(implicit ev: Show[T]): String = {
                  ev.show(value)
                }

                def addArg[T](key: String, value: T)(implicit ev: AddArg[T]): String = {
                  s"&$${ev.addArg(key, value)}"
                }

                def addPath[T](value: T)(implicit ev: AddPath[T]): String = {
                  ev.addPath(value)
                }
              }

              class Base64String(val data: Array[Byte]) extends AnyVal {
                override def toString() = "Base64String(" + data.toString() + ")"
              }
              object Base64String {
                def apply(bytes: Array[Byte]): Base64String = new Base64String(bytes)
                def unapply(value: Base64String): Option[Array[Byte]] = Some(value.data)
              }

            }
          """
      val path          = pkgPath.resolve("Implicits.scala")
      Target.pure(Some(WriteTree(path, sourceToBytes(path, implicits))))
    }
    def renderFrameworkImplicits(
        pkgPath: Path,
        pkgName: List[String],
        frameworkImports: List[scala.meta.Import],
        frameworkImplicitImportNames: List[scala.meta.Term.Name],
        jsonImports: List[scala.meta.Import],
        frameworkImplicits: scala.meta.Defn.Object,
        frameworkImplicitName: scala.meta.Term.Name
    ): Target[WriteTree] = {
      val pkg: Term.Ref            = pkgName.map(Term.Name.apply _).reduceLeft(Term.Select.apply _)
      val implicitsRef: Term.Ref   = (pkgName.map(Term.Name.apply _) ++ List(q"Implicits")).foldLeft[Term.Ref](q"_root_")(Term.Select.apply _)
      val frameworkImplicitImports = frameworkImplicitImportNames.map(name => q"import ${buildPkgTerm(List("_root_") ++ pkgName ++ List(name.value))}._")
      val frameworkImplicitsFile   = source"""
            package $pkg

            ..$jsonImports

            ..$frameworkImports

            import cats.implicits._
            import cats.data.EitherT

            import $implicitsRef._
            ..$frameworkImplicitImports

            $frameworkImplicits
          """
      val path                   = pkgPath.resolve(s"${frameworkImplicitName.value}.scala")
      Target.pure(WriteTree(path, sourceToBytes(path, frameworkImplicitsFile)))
    }
    def renderFrameworkDefinitions(
        pkgPath: Path,
        pkgName: List[String],
        frameworkImports: List[scala.meta.Import],
        frameworkDefinitions: List[scala.meta.Defn],
        frameworkDefinitionsName: scala.meta.Term.Name
    ): Target[WriteTree] = {
      val pkg: Term.Ref            = pkgName.map(Term.Name.apply _).reduceLeft(Term.Select.apply _)
      val frameworkDefinitionsFile = source"""
            package $pkg

            ..$frameworkImports

            ..$frameworkDefinitions
          """
      val path                     = pkgPath.resolve(s"${frameworkDefinitionsName.value}.scala")
      Target.pure(WriteTree(path, sourceToBytes(path, frameworkDefinitionsFile)))
    }

    def writePackageObject(
        dtoPackagePath: Path,
        pkgComponents: List[String],
        dtoComponents: Option[NonEmptyList[String]],
        customImports: List[scala.meta.Import],
        packageObjectImports: List[scala.meta.Import],
        protocolImports: List[scala.meta.Import],
        packageObjectContents: List[scala.meta.Stat],
        extraTypes: List[scala.meta.Stat]
    ): Target[Option[WriteTree]] = {
      val pkgImplicitsImport = q"import ${buildPkgTerm("_root_" +: pkgComponents)}.Implicits._"
      dtoComponents.traverse({
        case dtoComponents @ NonEmptyList(dtoHead, dtoRest) =>
          for (dtoRestNel <- Target.fromOption(NonEmptyList.fromList(dtoRest), UserError("DTO Components not quite long enough"))) yield {
            val dtoPkg = dtoRestNel.init.foldLeft[Term.Ref](Term.Name(dtoHead))({
              case (acc, next) =>
                Term.Select(acc, Term.Name(next))
            })
            val companion                 = Term.Name(s"${dtoRestNel.last}$$")
            val (_, statements)           = packageObjectContents.partition(partitionImplicits)
            val implicits: List[Defn.Val] = packageObjectContents.collect(matchImplicit)
            val mirroredImplicits = implicits.map { stat =>
              val List(Pat.Var(mirror)) = stat.pats
              stat.copy(rhs = q"$companion.$mirror")
            }
            val path = dtoPackagePath.resolve("package.scala")
            WriteTree(
              path,
              sourceToBytes(
                path,
                source"""
                package $dtoPkg

                ..${customImports ++ packageObjectImports ++ protocolImports :+ pkgImplicitsImport}

                object $companion {
                  ..${implicits.map(_.copy(mods = List.empty))}
                }

                package object ${Term.Name(dtoComponents.last)} {
                  ..${(mirroredImplicits ++ statements ++ extraTypes).to[List]}
                }
                """
              )
            )
          }
      })
    }

    def writeProtocolDefinition(
        outputPath: Path,
        pkgName: List[String],
        definitions: List[String],
        dtoComponents: List[String],
        imports: List[scala.meta.Import],
        protoImplicitName: Option[scala.meta.Term.Name],
        elem: StrictProtocolElems[ScalaLanguage]
    ): Target[(List[WriteTree], List[scala.meta.Stat])] = {
      val implicitImports = (List("Implicits") ++ protoImplicitName.map(_.value))
        .map(name => q"import ${buildPkgTerm(List("_root_") ++ pkgName ++ List(name))}._")
      Target.pure(elem match {
        case EnumDefinition(_, _, _, _, cls, staticDefns) =>
          val path = resolveFile(outputPath)(dtoComponents).resolve(s"${cls.name.value}.scala")
          (
            List(
              WriteTree(
                path,
                sourceToBytes(
                  path,
                  source"""
              package ${buildPkgTerm(dtoComponents)}
                ..$imports
                ..$implicitImports
                $cls
                ${companionForStaticDefns(staticDefns)}
              """
                )
              )
            ),
            List.empty[Stat]
          )
        case ClassDefinition(_, _, _, cls, staticDefns, _) =>
          val path = resolveFile(outputPath)(dtoComponents).resolve(s"${cls.name.value}.scala");
          (
            List(
              WriteTree(
                path,
                sourceToBytes(
                  path,
                  source"""
              package ${buildPkgTerm(dtoComponents)}
                ..$imports
                ..$implicitImports
                $cls
                ${companionForStaticDefns(staticDefns)}
              """
                )
              )
            ),
            List.empty[Stat]
          )
        case ADT(name, tpe, _, trt, staticDefns) =>
          val polyImports: Import = q"import cats.syntax.either._"
          val path                = resolveFile(outputPath)(dtoComponents).resolve(s"$name.scala")
          (
            List(
              WriteTree(
                path,
                sourceToBytes(
                  path,
                  source"""
                    package ${buildPkgTerm(dtoComponents)}
                    ..$imports
                    ..$implicitImports
                    $polyImports
                    $trt
                    ${companionForStaticDefns(staticDefns)}
                  """
                )
              )
            ),
            List.empty[Stat]
          )
        case RandomType(_, _) =>
          (List.empty, List.empty)
      })
    }

    def writeClient(
        pkgPath: Path,
        pkgName: List[String],
        customImports: List[scala.meta.Import],
        frameworkImplicitNames: List[scala.meta.Term.Name],
        dtoComponents: Option[List[String]],
        _client: Client[ScalaLanguage]
    ): Target[List[WriteTree]] = {
      val Client(pkg, clientName, imports, staticDefns, client, responseDefinitions) = _client
      val path                                                                       = resolveFile(pkgPath)(pkg :+ s"$clientName.scala")
      Target.pure(
        List(
          WriteTree(
            path,
            sourceToBytes(
              path,
              source"""
              package ${buildPkgTerm(pkgName ++ pkg)}
              import ${buildPkgTerm(List("_root_") ++ pkgName ++ List("Implicits"))}._
              ..${frameworkImplicitNames.map(name => q"import ${buildPkgTerm(List("_root_") ++ pkgName)}.$name._")}
              ..${dtoComponents.map(x => q"import ${buildPkgTerm(List("_root_") ++ x)}._")}
              ..$customImports;
              ..$imports;
              ${companionForStaticDefns(staticDefns)};
              ..${client.toList.map(_.merge)};
              ..$responseDefinitions
              """
            )
          )
        )
      )
    }
    def writeServer(
        pkgPath: Path,
        pkgName: List[String],
        customImports: List[scala.meta.Import],
        frameworkImplicitNames: List[scala.meta.Term.Name],
        dtoComponents: Option[List[String]],
        server: Server[ScalaLanguage]
    ): Target[List[WriteTree]] = {
      val Server(pkg, extraImports, handlerDefinition, serverDefinitions) = server
      val path                                                            = resolveFile(pkgPath)(pkg.toList :+ "Routes.scala")
      Target.pure(
        List(
          WriteTree(
            path,
            sourceToBytes(
              path,
              source"""
              package ${buildPkgTerm(pkgName ++ pkg.toList)}
              ..$extraImports
              import ${buildPkgTerm(List("_root_") ++ pkgName ++ List("Implicits"))}._
              ..${frameworkImplicitNames.map(name => q"import ${buildPkgTerm(List("_root_") ++ pkgName)}.$name._")}
              ..${dtoComponents.map(x => q"import ${buildPkgTerm(List("_root_") ++ x)}._")}
              ..$customImports
              $handlerDefinition
              ..$serverDefinitions
              """
            )
          )
        )
      )
    }

    def wrapToObject(name: scala.meta.Term.Name, imports: List[scala.meta.Import], definitions: List[scala.meta.Defn]): Target[Option[scala.meta.Defn.Object]] =
      Target.pure(Some(q"""
             object $name {
                 ..$imports
                 ..$definitions
             }
           """))
  }
}
