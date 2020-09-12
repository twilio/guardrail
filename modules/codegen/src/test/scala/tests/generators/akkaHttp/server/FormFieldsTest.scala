package tests.generators.akkaHttp.server

import com.twilio.guardrail.generators.Scala.AkkaHttp
import com.twilio.guardrail.{ Context, Server, Servers }
import support.SwaggerSpecRunner
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FormFieldsServerTest extends AnyFunSuite with Matchers with SwaggerSpecRunner {
  import scala.meta._

  val swagger: String = s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |schemes:
    |  - http
    |paths:
    |  /foo:
    |    put:
    |      operationId: putFoo
    |      consumes:
    |        - multipart/form-data
    |      parameters:
    |        - name: foo
    |          in: formData
    |          type: string
    |          required: true
    |        - name: bar
    |          in: formData
    |          type: integer
    |          format: int64
    |          required: true
    |        - name: baz
    |          in: formData
    |          type: file
    |          required: true
    |          x-scala-file-hash: SHA-512
    |      responses:
    |        200:
    |          description: Success
    |""".stripMargin

  test("Ensure routes are generated") {
    val (
      _,
      _,
      Servers(Server(pkg, extraImports, genHandler, genResource :: Nil) :: Nil, Nil)
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)

    val handler  = q"""
      trait Handler {
        def putFoo(respond: Resource.PutFooResponse.type)(foo: String, bar: Long, baz: (File, Option[String], ContentType, String)): scala.concurrent.Future[Resource.PutFooResponse]
        def putFooMapFileField(fieldName: String, fileName: Option[String], contentType: ContentType): File
        def putFooUnmarshalToFile[F[_]: Functor](hashType: F[String], destFn: (String, Option[String], ContentType) => File)(implicit mat: Materializer): Unmarshaller[Multipart.FormData.BodyPart, (File, Option[String], ContentType, F[String])] = Unmarshaller { implicit executionContext =>
          part => {
            val dest = destFn(part.name, part.filename, part.entity.contentType)
            val messageDigest = hashType.map(MessageDigest.getInstance(_))
            val fileSink: Sink[ByteString, Future[IOResult]] = FileIO.toPath(dest.toPath).contramap[ByteString] { chunk =>
              val _ = messageDigest.map(_.update(chunk.toArray[Byte]))
              chunk
            }
            part.entity.dataBytes.toMat(fileSink)(Keep.right).run().transform({
              case IOResult(_, Success(_)) =>
                val hash = messageDigest.map(md => javax.xml.bind.DatatypeConverter.printHexBinary(md.digest()).toLowerCase(java.util.Locale.US))
                (dest, part.filename, part.entity.contentType, hash)
              case IOResult(_, Failure(t)) =>
                dest.delete()
                throw t
            }, {
              case t =>
                dest.delete()
                t
            })
          }
        }
      }
    """
    val resource = q"""
      object Resource {
        def routes(handler: Handler)(implicit mat: akka.stream.Materializer): Route = {
          {
            path("foo")(put(({
              object putFooParts {
                sealed trait Part
                case class IgnoredPart(unit: Unit) extends Part
                case class foo(value: String) extends Part
                case class bar(value: Long) extends Part
                case class baz(value: (File, Option[String], ContentType, String)) extends Part
              }
              val UnmarshalfooPart: Unmarshaller[Multipart.FormData.BodyPart, putFooParts.foo] = Unmarshaller { implicit executionContext =>
                part => Unmarshaller.firstOf(MFDBPviaFSU(stringyJsonEntityUnmarshaller.andThen(unmarshallJson[String])), MFDBPviaFSU(structuredJsonEntityUnmarshaller.andThen(unmarshallJson[String]))).apply(part).map(putFooParts.foo.apply).recoverWith({
                  case ex =>
                    Future.failed(RejectionError(MalformedFormFieldRejection(part.name, ex.getMessage, Some(ex))))
                })
              }
              val UnmarshalbarPart: Unmarshaller[Multipart.FormData.BodyPart, putFooParts.bar] = Unmarshaller { implicit executionContext =>
                part => Unmarshaller.firstOf(MFDBPviaFSU(sneakyJsonEntityUnmarshaller.andThen(unmarshallJson[Long])), MFDBPviaFSU(structuredJsonEntityUnmarshaller.andThen(unmarshallJson[Long]))).apply(part).map(putFooParts.bar.apply).recoverWith({
                  case ex =>
                    Future.failed(RejectionError(MalformedFormFieldRejection(part.name, ex.getMessage, Some(ex))))
                })
              }
              val UnmarshalbazPart: Unmarshaller[Multipart.FormData.BodyPart, putFooParts.baz] = handler.putFooUnmarshalToFile[Id]("SHA-512", handler.putFooMapFileField(_, _, _)).map({
                case (v1, v2, v3, v4) =>
                  putFooParts.baz((v1, v2, v3, v4))
              })
              val fileReferences = new AtomicReference(List.empty[File])
              implicit val MultipartFormDataUnmarshaller: FromRequestUnmarshaller[Either[Throwable, (Option[String], Option[Long], Option[(File, Option[String], ContentType, String)])]] = implicitly[FromRequestUnmarshaller[Multipart.FormData]].flatMap { implicit executionContext => implicit mat => formData =>
                val collectedPartsF: Future[Either[Throwable, (Option[String], Option[Long], Option[(File, Option[String], ContentType, String)])]] = for (results <- formData.parts.mapConcat {
                  part => if (Set[String]("foo", "bar", "baz").contains(part.name)) part :: Nil else {
                    part.entity.discardBytes()
                    Nil
                  }
                }.mapAsync(1) {
                  part => part.name match {
                    case "foo" =>
                      SafeUnmarshaller(UnmarshalfooPart).apply(part)
                    case "bar" =>
                      SafeUnmarshaller(UnmarshalbarPart).apply(part)
                    case "baz" =>
                      SafeUnmarshaller(AccumulatingUnmarshaller(fileReferences, UnmarshalbazPart)(_.value._1)).apply(part)
                    case _ =>
                      SafeUnmarshaller(implicitly[Unmarshaller[Multipart.FormData.BodyPart, Unit]].map(putFooParts.IgnoredPart.apply(_))).apply(part)
                  }
                }.toMat(Sink.seq[Either[Throwable, putFooParts.Part]])(Keep.right).run()) yield {
                  results.toList.sequence.map { successes =>
                    val fooO = successes.collectFirst({
                      case putFooParts.foo(v1) => v1
                    })
                    val barO = successes.collectFirst({
                      case putFooParts.bar(v1) => v1
                    })
                    val bazO = successes.collectFirst({
                      case putFooParts.baz((v1, v2, v3, v4)) =>
                        (v1, v2, v3, v4)
                    })
                    (fooO, barO, bazO)
                  }
                }
                collectedPartsF
              }
              (handleExceptions(ExceptionHandler({
                case e: Throwable =>
                  fileReferences.get().foreach(_.delete())
                  throw e
              })) & extractSettings.flatMap {
                settings => handleRejections { (rejections: scala.collection.immutable.Seq[Rejection]) =>
                  fileReferences.get().foreach(_.delete())
                  rejections.collectFirst({
                    case MalformedRequestContentRejection(msg, EntityStreamSizeException(limit, contentLength)) =>
                      val summary = contentLength match {
                        case Some(cl) =>
                          s"Request Content-Length of $$cl bytes exceeds the configured limit of $$limit bytes"
                        case None =>
                          s"Aggregated data length of request entity exceeds the configured limit of $$limit bytes"
                      }
                      val info = new ErrorInfo(summary, "Consider increasing the value of akka.http.server.parsing.max-content-length")
                      val status = StatusCodes.PayloadTooLarge
                      val msg = if (settings.verboseErrorMessages) info.formatPretty else info.summary
                      complete(HttpResponse(status, entity = msg))
                  })
                }
              } & mapResponse { resp =>
                fileReferences.get().foreach(_.delete())
                resp
              } & entity(as(Unmarshaller.firstOf(MultipartFormDataUnmarshaller)))).flatMap(_.fold({
                case RejectionError(rej) =>
                  reject(rej)
                case t =>
                  throw t
              }, {
                case (fooO, barO, bazO) =>
                  val maybe: Either[Rejection, (String, Long, (File, Option[String], ContentType, String))] = for (foo <- fooO.toRight(MissingFormFieldRejection("foo")); bar <- barO.toRight(MissingFormFieldRejection("bar")); baz <- bazO.toRight(MissingFormFieldRejection("baz"))) yield {
                    (foo, bar, baz)
                  }
                  maybe.fold(reject(_), tprovide(_))
              }))
            }: Directive[(String, Long, (File, Option[String], ContentType, String))]).apply((foo, bar, baz) => complete(handler.putFoo(PutFooResponse)(foo, bar, baz)))))
          }
        }
        sealed abstract class PutFooResponse(val statusCode: StatusCode)
        case object PutFooResponseOK extends PutFooResponse(StatusCodes.OK)
        object PutFooResponse {
          implicit def putFooResponseTRM: ToResponseMarshaller[PutFooResponse] = Marshaller { implicit ec =>
            resp => putFooResponseTR(resp)
          }
          implicit def putFooResponseTR(value: PutFooResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r: PutFooResponseOK.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
          }
          def apply[T](value: T)(implicit ev: T => PutFooResponse): PutFooResponse = ev(value)
          def OK: PutFooResponse = PutFooResponseOK
        }
      }
    """

    genHandler.structure shouldEqual handler.structure
    genResource.structure shouldEqual resource.structure
  }
}
