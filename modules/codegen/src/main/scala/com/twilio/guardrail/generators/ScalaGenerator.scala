package com.twilio.guardrail.generators

import cats.~>
import cats.data.NonEmptyList
import cats.implicits._
import com.twilio.guardrail._
import com.twilio.guardrail.Common.resolveFile
import com.twilio.guardrail.generators.syntax.RichString
import com.twilio.guardrail.generators.syntax.Scala._
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.terms._
import java.nio.charset.StandardCharsets

import scala.meta._

object ScalaGenerator {
  private def sourceToBytes(source: Source): Array[Byte] = (GENERATED_CODE_COMMENT + source.syntax).getBytes(StandardCharsets.UTF_8)

  object ScalaInterp extends (ScalaTerm[ScalaLanguage, ?] ~> Target) {
    // TODO: Very interesting bug. 2.11.12 barfs if these two definitions are
    // defined inside `apply`. Once 2.11 is dropped, these can be moved back.
    val matchImplicit: PartialFunction[Stat, Defn.Val] = {
      case x: Defn.Val if (x match { case q"implicit val $_: $_ = $_" => true; case _ => false }) => x
    }
    val partitionImplicits: PartialFunction[Stat, Boolean] = matchImplicit.andThen(_ => true).orElse({ case _ => false })

    val buildPkgTerm: List[String] => Term.Ref =
      _.map(Term.Name.apply _).reduceLeft(Term.Select.apply _)

    def apply[T](term: ScalaTerm[ScalaLanguage, T]): Target[T] = term match {
      case VendorPrefixes() => Target.pure(List("x-scala", "x-jvm"))

      case LitString(value)                    => Target.pure(Lit.String(value))
      case LitFloat(value)                     => Target.pure(Lit.Float(value))
      case LitDouble(value)                    => Target.pure(Lit.Double(value))
      case LitInt(value)                       => Target.pure(Lit.Int(value))
      case LitLong(value)                      => Target.pure(Lit.Long(value))
      case LitBoolean(value)                   => Target.pure(Lit.Boolean(value))
      case LiftOptionalType(value)             => Target.pure(t"Option[${value}]")
      case LiftOptionalTerm(value)             => Target.pure(q"Option(${value})")
      case EmptyOptionalTerm()                 => Target.pure(q"None")
      case EmptyArray()                        => Target.pure(q"Vector.empty")
      case EmptyMap()                          => Target.pure(q"Map.empty")
      case LiftVectorType(value, customTpe)    => Target.pure(t"${customTpe.getOrElse(t"Vector")}[${value}]")
      case LiftVectorTerm(value)               => Target.pure(q"Vector(${value})")
      case LiftMapType(value, customTpe)       => Target.pure(t"${customTpe.getOrElse(t"Map")}[String, ${value}]")
      case FullyQualifyPackageName(rawPkgName) => Target.pure("_root_" +: rawPkgName)
      case LookupEnumDefaultValue(tpe, defaultValue, values) => {
        // FIXME: Is there a better way to do this? There's a gap of coverage here
        defaultValue match {
          case Lit.String(name) =>
            values
              .find(_._1 == name)
              .fold(Target.raiseError[Term.Select](s"Enumeration ${tpe} is not defined for default value ${name}"))(value => Target.pure(value._3))
          case _ =>
            Target.raiseError[Term.Select](s"Enumeration ${tpe} somehow has a default value that isn't a string")
        }
      }
      case FormatEnumName(enumValue) => Target.pure(enumValue.toPascalCase)
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
        (tpe match {
          case SwaggerUtil.Deferred(inner) => Target.pure(SwaggerUtil.DeferredMap(inner, containerTpe))
          case SwaggerUtil.DeferredMap(_, _) =>
            Target.raiseError("FIXME: Got a map of maps, currently not supported")
          case SwaggerUtil.DeferredArray(_, _) =>
            Target.raiseError("FIXME: Got a map of arrays, currently not supported")
        })
      case ParseType(tpe) =>
        Target.pure(
          tpe
            .parse[Type]
            .fold({ err =>
              println(s"Warning: Unparsable x-scala-type: ${tpe} ${err}")
              None
            }, Option.apply _)
        )
      case ParseTypeName(tpe) =>
        Target.pure(Option(tpe.trim).filterNot(_.isEmpty).map(Type.Name(_)))

      case PureTermName(tpe) =>
        Target.fromOption(Option(tpe.trim).filterNot(_.isEmpty).map(Term.Name(_)), "A structure's name is empty")

      case PureTypeName(tpe) =>
        Target.fromOption(Option(tpe.trim).filterNot(_.isEmpty).map(Type.Name(_)), "A structure's name is empty")

      case PureMethodParameter(name, tpe, default) =>
        Target.pure(param"${name}: ${tpe}".copy(default = default))

      case TypeNamesEqual(a, b) =>
        Target.pure(a.value == b.value)

      case TypesEqual(a, b) =>
        Target.pure(a.structure == b.structure)

      case ExtractTypeName(tpe) =>
        Target.pure(tpe match {
          case x: Type.Name => Option(x)
          case _            => Option.empty
        })
      case ExtractTermName(term) =>
        val Term.Name(name) = term
        Target.pure(name)

      case SelectType(typeNames) =>
        val tpe   = Type.Name(typeNames.last)
        val names = typeNames.init.map(Term.Name.apply _)

        val result = names match {
          case Nil => tpe
          case x :: xs =>
            val term = xs.foldLeft[Term.Ref](x)(Term.Select.apply _)
            Type.Select(term, tpe)
        }

        Target.pure(result)

      case SelectTerm(termNames) =>
        val result = termNames.tail.foldLeft[Term](Term.Name(termNames.head)) {
          case (current, next) => Term.Select(current, Term.Name(next))
        }
        Target.pure(result)

      case AlterMethodParameterName(param, name) =>
        Target.pure(param.copy(name = name))

      case BytesType()               => Target.pure(t"Base64String")
      case DateType()                => Target.pure(t"java.time.LocalDate")
      case DateTimeType()            => Target.pure(t"java.time.OffsetDateTime")
      case UUIDType()                => Target.pure(t"java.util.UUID")
      case StringType(format)        => Target.pure(format.fold(t"String")(Type.Name(_)))
      case FloatType()               => Target.pure(t"Float")
      case DoubleType()              => Target.pure(t"Double")
      case NumberType(format)        => Target.pure(t"BigDecimal")
      case IntType()                 => Target.pure(t"Int")
      case LongType()                => Target.pure(t"Long")
      case IntegerType(format)       => Target.pure(t"BigInt")
      case BooleanType(format)       => Target.pure(t"Boolean")
      case ArrayType(format)         => Target.pure(t"Iterable[String]")
      case FallbackType(tpe, format) => Target.fromOption(tpe, "Missing type").map(Type.Name(_))

      case WidenTypeName(tpe)           => Target.pure(tpe)
      case WidenTermSelect(value)       => Target.pure(value)
      case WidenClassDefinition(value)  => Target.pure(value)
      case WidenObjectDefinition(value) => Target.pure(value)

      case FindCommonDefaultValue(history, a, b) =>
        (a, b) match {
          case (Some(va), Some(vb)) =>
            if (va.structure == vb.structure) {
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
        val pkg: Term.Ref =
          pkgName.map(Term.Name.apply _).reduceLeft(Term.Select.apply _)

        val implicits = source"""
            package ${pkg}

            ..${jsonImports}

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

                implicit def addArgSeq[T](implicit ev: AddArg[T]): AddArg[List[T]] = build[List[T]](key => vs => vs.map(v => ev.addArg(key, v)).mkString("&"))
                implicit def addArgIterable[T](implicit ev: AddArg[T]): AddArg[Iterable[T]] = build[Iterable[T]](key => vs => vs.map(v => ev.addArg(key, v)).mkString("&"))
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
                private[this] val encoder = java.util.Base64.getEncoder
                implicit val encode: Encoder[Base64String] =
                  Encoder[String].contramap[Base64String](v => new String(encoder.encode(v.data)))

                private[this] val decoder = java.util.Base64.getDecoder
                implicit val decode: Decoder[Base64String] =
                  Decoder[String].emapTry(v => scala.util.Try(decoder.decode(v))).map(new Base64String(_))
              }

            }
          """
        Target.pure(Some(WriteTree(pkgPath.resolve("Implicits.scala"), sourceToBytes(implicits))))

      case RenderFrameworkImplicits(pkgPath, pkgName, frameworkImports, jsonImports, frameworkImplicits, frameworkImplicitName) =>
        val pkg: Term.Ref =
          pkgName.map(Term.Name.apply _).reduceLeft(Term.Select.apply _)
        val implicitsRef: Term.Ref =
          (pkgName.map(Term.Name.apply _) ++ List(q"Implicits")).foldLeft[Term.Ref](q"_root_")(Term.Select.apply _)
        val frameworkImplicitsFile = source"""
            package ${pkg}

            ..${jsonImports}

            ..${frameworkImports}

            import cats.implicits._
            import cats.data.EitherT

            import ${implicitsRef}._

            ${frameworkImplicits}
          """
        Target.pure(WriteTree(pkgPath.resolve(s"${frameworkImplicitName.value}.scala"), sourceToBytes(frameworkImplicitsFile)))

      case RenderFrameworkDefinitions(pkgPath, pkgName, frameworkImports, frameworkDefinitions, frameworkDefinitionsName) =>
        val pkg: Term.Ref =
          pkgName.map(Term.Name.apply _).reduceLeft(Term.Select.apply _)
        val implicitsRef: Term.Ref =
          (pkgName.map(Term.Name.apply _) ++ List(q"Implicits")).foldLeft[Term.Ref](q"_root_")(Term.Select.apply _)
        val frameworkDefinitionsFile = source"""
            package ${pkg}

            ..${frameworkImports}

            import cats.implicits._
            import cats.data.EitherT

            import ${implicitsRef}._

            ${frameworkDefinitions}
          """
        Target.pure(WriteTree(pkgPath.resolve(s"${frameworkDefinitionsName.value}.scala"), sourceToBytes(frameworkDefinitionsFile)))

      case WritePackageObject(dtoPackagePath, dtoComponents, customImports, packageObjectImports, protocolImports, packageObjectContents, extraTypes) =>
        dtoComponents.traverse {
          case dtoComponents @ NonEmptyList(dtoHead, dtoRest) =>
            for {
              dtoRestNel <- Target.fromOption(NonEmptyList.fromList(dtoRest), "DTO Components not quite long enough")
            } yield {
              val dtoPkg = dtoRestNel.init
                .foldLeft[Term.Ref](Term.Name(dtoHead)) {
                  case (acc, next) => Term.Select(acc, Term.Name(next))
                }
              val companion = Term.Name(s"${dtoRestNel.last}$$")

              val (_, statements) =
                packageObjectContents.partition(partitionImplicits)
              val implicits: List[Defn.Val] = packageObjectContents.collect(matchImplicit)

              val mirroredImplicits = implicits
                .map({ stat =>
                  val List(Pat.Var(mirror)) = stat.pats
                  stat.copy(rhs = q"${companion}.${mirror}")
                })

              WriteTree(
                dtoPackagePath.resolve("package.scala"),
                sourceToBytes(source"""
                package ${dtoPkg}

                ..${customImports ++ packageObjectImports ++ protocolImports}

                object ${companion} {
                  ..${implicits.map(_.copy(mods = List.empty))}
                }

                package object ${Term.Name(dtoComponents.last)} {
                  ..${(mirroredImplicits ++ statements ++ extraTypes).to[List]}
                }
                """)
              )
            }
        }
      case WriteProtocolDefinition(outputPath, pkgName, definitions, dtoComponents, imports, elem) =>
        Target.pure(elem match {
          case EnumDefinition(_, _, _, _, cls, staticDefns) =>
            (
              List(
                WriteTree(
                  resolveFile(outputPath)(dtoComponents).resolve(s"${cls.name.value}.scala"),
                  sourceToBytes(source"""
              package ${buildPkgTerm(dtoComponents)}
                import ${buildPkgTerm(List("_root_") ++ pkgName ++ List("Implicits"))}._;
                ..${imports}

                $cls
                ${companionForStaticDefns(staticDefns)}
              """)
                )
              ),
              List.empty[Stat]
            )

          case ClassDefinition(_, _, _, cls, staticDefns, _) =>
            (
              List(
                WriteTree(
                  resolveFile(outputPath)(dtoComponents).resolve(s"${cls.name.value}.scala"),
                  sourceToBytes(source"""
              package ${buildPkgTerm(dtoComponents)}
                ..${imports}
                import ${buildPkgTerm(List("_root_") ++ pkgName ++ List("Implicits"))}._;
                $cls
                ${companionForStaticDefns(staticDefns)}
              """)
                )
              ),
              List.empty[Stat]
            )

          case ADT(name, tpe, _, trt, staticDefns) =>
            val polyImports: Import = q"""import cats.syntax.either._"""

            (
              List(
                WriteTree(
                  resolveFile(outputPath)(dtoComponents).resolve(s"$name.scala"),
                  sourceToBytes(source"""
                    package ${buildPkgTerm(dtoComponents)}

                    ..$imports
                    $polyImports
                    $trt
                    ${companionForStaticDefns(staticDefns)}
                  """)
                )
              ),
              List.empty[Stat]
            )

          case RandomType(_, _) =>
            (List.empty, List.empty)
        })
      case WriteClient(
          pkgPath,
          pkgName,
          customImports,
          frameworkImplicitName,
          dtoComponents,
          Client(pkg, clientName, imports, staticDefns, client, responseDefinitions)
          ) =>
        Target.pure(
          List(
            WriteTree(
              resolveFile(pkgPath)(pkg :+ s"${clientName}.scala"),
              sourceToBytes(source"""
              package ${buildPkgTerm(pkgName ++ pkg)}
              import ${buildPkgTerm(List("_root_") ++ pkgName ++ List("Implicits"))}._
              ..${frameworkImplicitName.map(name => q"import ${buildPkgTerm(List("_root_") ++ pkgName)}.${name}._")}
              ..${dtoComponents.map(x => q"import ${buildPkgTerm(List("_root_") ++ x)}._")}
              ..${customImports};
              ..${imports};
              ${companionForStaticDefns(staticDefns)};
              ..${client.toList.map(_.merge)};
              ..${responseDefinitions}
              """)
            )
          )
        )
      case WriteServer(
          pkgPath,
          pkgName,
          customImports,
          frameworkImplicitName,
          dtoComponents,
          Server(pkg, extraImports, handlerDefinition, serverDefinitions)
          ) =>
        Target.pure(
          List(
            WriteTree(
              resolveFile(pkgPath)(pkg.toList :+ "Routes.scala"),
              sourceToBytes(source"""
              package ${buildPkgTerm((pkgName ++ pkg.toList))}
              ..${extraImports}
              import ${buildPkgTerm(List("_root_") ++ pkgName ++ List("Implicits"))}._
              ..${frameworkImplicitName.map(name => q"import ${buildPkgTerm(List("_root_") ++ pkgName)}.${name}._")}
              ..${dtoComponents.map(x => q"import ${buildPkgTerm(List("_root_") ++ x)}._")}
              ..${customImports}
              ${handlerDefinition}
              ..${serverDefinitions}
              """)
            )
          )
        )
      case WrapToObject(name, imports, definitions) =>
        Target.pure(Option(q"""
             object $name {
                 ..$imports
                 ..$definitions
             }
           """))
    }
  }
}
