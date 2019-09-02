package com.twilio.guardrail
package core

import cats.data.{ NonEmptyList, State }
import cats.implicits._
import cats.{ FlatMap, ~> }
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.terms._
import java.nio.file.Paths
import scala.util.control.NonFatal

case class CoreTermInterp[L <: LA](defaultFramework: String,
                                   frameworkMapping: PartialFunction[String, CodegenApplication[L, ?] ~> Target],
                                   handleImport: String => Either[Error, L#Import])
    extends (CoreTerm[L, ?] ~> CoreTarget) {
  def apply[T](x: CoreTerm[L, T]): CoreTarget[T] = x match {
    case GetDefaultFramework() =>
      CoreTarget.log.function("getDefaultFramework") {
        Target.log.debug(s"Providing ${defaultFramework}") >> CoreTarget.pure(Some(defaultFramework))
      }

    case ExtractGenerator(context, vendorDefaultFramework) =>
      CoreTarget.log.function("extractGenerator") {
        for {
          _             <- CoreTarget.log.debug("Looking up framework")
          frameworkName <- CoreTarget.fromOption(context.framework.orElse(vendorDefaultFramework), NoFramework)
          framework     <- CoreTarget.fromOption(PartialFunction.condOpt(frameworkName)(frameworkMapping), UnknownFramework(frameworkName))
          _             <- CoreTarget.log.debug(s"Found: $framework")
        } yield framework
      }

    case ValidateArgs(parsed) =>
      for {
        args <- CoreTarget.pure(parsed.filterNot(_.defaults))
        args <- CoreTarget.fromOption(NonEmptyList.fromList(args.filterNot(Args.isEmpty)), NoArgsSpecified)
        args <- if (args.exists(_.printHelp))
          CoreTarget.raiseError[NonEmptyList[Args]](PrintHelp)
        else CoreTarget.pure(args)
      } yield args

    case ParseArgs(args) => {
      def expandTilde(path: String): String =
        path.replaceFirst("^~", System.getProperty("user.home"))
      val defaultArgs =
        Args.empty.copy(context = Args.empty.context, defaults = true)

      type From = (List[Args], List[String])
      type To   = List[Args]
      val start: From = (List.empty[Args], args.toList)
      import CoreTarget.log.debug
      Target.log.function("parseArgs") {
        FlatMap[CoreTarget].tailRecM[From, To](start)({
          case pair @ (sofar, rest) =>
            val empty = sofar
              .filter(_.defaults)
              .reverse
              .headOption
              .getOrElse(defaultArgs)
              .copy(defaults = false)
            def Continue(x: From): CoreTarget[Either[From, To]] = CoreTarget.pure(Either.left(x))
            def Return(x: To): CoreTarget[Either[From, To]]     = CoreTarget.pure(Either.right(x))
            def Bail(x: Error): CoreTarget[Either[From, To]]    = CoreTarget.raiseError(x)
            for {
              _ <- debug(s"Processing: ${rest.take(5).mkString(" ")}${if (rest.length > 3) "..." else ""} of ${rest.length}")
              step <- pair match {
                case (already, Nil) =>
                  debug("Finished") >> Return(already)
                case (Nil, xs @ (_ :: _)) => Continue((empty :: Nil, xs))
                case (sofar :: already, "--defaults" :: xs) =>
                  Continue((empty.copy(defaults = true) :: sofar :: already, xs))
                case (sofar :: already, "--client" :: xs) =>
                  Continue((empty :: sofar :: already, xs))
                case (sofar :: already, "--server" :: xs) =>
                  Continue((empty.copy(kind = CodegenTarget.Server) :: sofar :: already, xs))
                case (sofar :: already, "--models" :: xs) =>
                  Continue((empty.copy(kind = CodegenTarget.Models) :: sofar :: already, xs))
                case (sofar :: already, "--framework" :: value :: xs) =>
                  Continue((sofar.copy(context = sofar.context.copy(framework = Some(value))) :: already, xs))
                case (sofar :: already, "--help" :: xs) =>
                  Continue((sofar.copy(printHelp = true) :: already, List.empty))
                case (sofar :: already, "--specPath" :: value :: xs) =>
                  Continue((sofar.copy(specPath = Option(expandTilde(value))) :: already, xs))
                case (sofar :: already, "--tracing" :: xs) =>
                  Continue((sofar.copy(context = sofar.context.copy(tracing = true)) :: already, xs))
                case (sofar :: already, "--outputPath" :: value :: xs) =>
                  Continue((sofar.copy(outputPath = Option(expandTilde(value))) :: already, xs))
                case (sofar :: already, "--packageName" :: value :: xs) =>
                  Continue((sofar.copy(packageName = Option(value.trim.split('.').to[List])) :: already, xs))
                case (sofar :: already, "--dtoPackage" :: value :: xs) =>
                  Continue((sofar.copy(dtoPackage = value.trim.split('.').to[List]) :: already, xs))
                case (sofar :: already, "--import" :: value :: xs) =>
                  Continue((sofar.copy(imports = sofar.imports :+ value) :: already, xs))
                case (sofar :: already, "--http4sAuthedRoutes" :: xs) =>
                  Continue((sofar.copy(context = sofar.context.copy(http4sAuthedRoutes = true)) :: already, xs))
                case (_, unknown) =>
                  debug("Unknown argument") >> Bail(UnknownArguments(unknown))
              }
            } yield step
        })
      }
    }

    case ProcessArgSet(targetInterpreter, args) =>
      import scala.meta.parsers.Parsed
      implicit def parsed2Either[Z]: Parsed[Z] => Either[Parsed.Error, Z] = {
        case x: Parsed.Error      => Left(x)
        case Parsed.Success(tree) => Right(tree)
      }
      Target.log.function("processArgSet")(for {
        _          <- CoreTarget.log.debug("Processing arguments")
        specPath   <- CoreTarget.fromOption(args.specPath, MissingArg(args, Error.ArgName("--specPath")))
        outputPath <- CoreTarget.fromOption(args.outputPath, MissingArg(args, Error.ArgName("--outputPath")))
        pkgName    <- CoreTarget.fromOption(args.packageName, MissingArg(args, Error.ArgName("--packageName")))
        kind       = args.kind
        dtoPackage = args.dtoPackage
        context    = args.context
        customImports <- args.imports
          .traverse(
            x =>
              for {
                _ <- CoreTarget.log.debug(s"Attempting to parse $x as an import directive")
                customImport <- handleImport(x)
                  .fold[CoreTarget[L#Import]](err => CoreTarget.raiseError(UnparseableArgument("import", err.toString)), CoreTarget.pure _)
              } yield customImport
          )
        _ <- CoreTarget.log.debug("Finished processing arguments")
      } yield {
        ReadSwagger(
          Paths.get(specPath), {
            swagger =>
              try {
                (for {
                  defs <- Common.prepareDefinitions[L, CodegenApplication[L, ?]](kind, context, swagger)
                  (proto, codegen) = defs
                  result <- Common
                    .writePackage[L, CodegenApplication[L, ?]](proto, codegen, context)(Paths.get(outputPath), pkgName, dtoPackage, customImports)
                } yield result).foldMap(targetInterpreter)
              } catch {
                case NonFatal(ex) =>
                  val stackTrace =
                    ex.getStackTrace()
                      .toList
                      .foldLeftM[State[Option[String], ?], List[String]](List.empty)({
                        case (acc, next) =>
                          for {
                            lastClassName <- State.get
                            _             <- State.set(Option(next.getClassName()))
                          } yield {
                            if (next.getClassName().startsWith("com.twilio")) {
                              acc :+ s"        at ${next.toString()}"
                            } else {
                              if (lastClassName.exists(_.startsWith("com.twilio"))) {
                                acc :+ "          ..."
                              } else acc
                            }
                          }
                      })
                      .runA(Option.empty)
                      .value
                  Target.raiseException(s"""
                    |Error attempting to process ${specPath}:
                    |
                    |${ex.toString()}
                    |${stackTrace.mkString("\n")}
                    |""".stripMargin.trim)
              }
          }
        )
      })
  }
}
