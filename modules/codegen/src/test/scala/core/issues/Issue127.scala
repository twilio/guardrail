package tests.core.issues

import com.twilio.guardrail.generators.Scala.AkkaHttp
import com.twilio.guardrail.{ Context, Server, Servers }
import support.SwaggerSpecRunner

import scala.meta._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Issue127 extends AnyFunSuite with Matchers with SwaggerSpecRunner {
  val swagger: String = s"""
    |swagger: '2.0'
    |host: localhost:1234
    |schemes:
    |- http
    |paths:
    |  /file:
    |    post:
    |      operationId: uploadFile
    |      parameters:
    |      - name: file
    |        description: File to upload
    |        in: formData
    |        type: file
    |        required: true
    |      consumes:
    |      - multipart/form-data
    |      responses:
    |        '201':
    |          description: Success
    |""".stripMargin

  test("Generate plain array alias definition") {
    val (
      _,
      _,
      Servers(Server(pkg, extraImports, genHandler, genResource :: Nil) :: Nil, Nil)
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)

    val handler = q"""
      trait Handler {
        def uploadFile(respond: Resource.UploadFileResponse.type)(file: (File, Option[String], ContentType)): scala.concurrent.Future[Resource.UploadFileResponse]
        def uploadFileMapFileField(fieldName: String, fileName: Option[String], contentType: ContentType): File
        def uploadFileUnmarshalToFile[F[_]: Functor](hashType: F[String], destFn: (String, Option[String], ContentType) => File)(implicit mat: Materializer): Unmarshaller[Multipart.FormData.BodyPart, (File, Option[String], ContentType, F[String])] = Unmarshaller { implicit executionContext =>
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
            path("file")(post(({
              object uploadFileParts {
                sealed trait Part
                case class IgnoredPart(unit: Unit) extends Part
                case class file(value: (File, Option[String], ContentType)) extends Part
              }
              val UnmarshalfilePart: Unmarshaller[Multipart.FormData.BodyPart, uploadFileParts.file] = handler.uploadFileUnmarshalToFile[Option](None, handler.uploadFileMapFileField(_, _, _)).map({
                case (v1, v2, v3, v4) =>
                  uploadFileParts.file((v1, v2, v3))
              })
              val fileReferences = new AtomicReference(List.empty[File])
              implicit val MultipartFormDataUnmarshaller: FromRequestUnmarshaller[Either[Throwable, Tuple1[Option[(File, Option[String], ContentType)]]]] = implicitly[FromRequestUnmarshaller[Multipart.FormData]].flatMap { implicit executionContext => implicit mat => formData =>
                val collectedPartsF: Future[Either[Throwable, Tuple1[Option[(File, Option[String], ContentType)]]]] = for (results <- formData.parts.mapConcat {
                  part => if (Set[String]("file").contains(part.name)) part :: Nil else {
                    part.entity.discardBytes()
                    Nil
                  }
                }.mapAsync(1) {
                  part => part.name match {
                    case "file" =>
                      SafeUnmarshaller(AccumulatingUnmarshaller(fileReferences, UnmarshalfilePart)(_.value._1)).apply(part)
                    case _ =>
                      SafeUnmarshaller(implicitly[Unmarshaller[Multipart.FormData.BodyPart, Unit]].map(uploadFileParts.IgnoredPart.apply(_))).apply(part)
                  }
                }.toMat(Sink.seq[Either[Throwable, uploadFileParts.Part]])(Keep.right).run()) yield {
                  results.toList.sequence.map { successes =>
                    val fileO = successes.collectFirst({
                      case uploadFileParts.file((v1, v2, v3)) =>
                        (v1, v2, v3)
                    })
                    Tuple1(fileO)
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
                case Tuple1(fileO) =>
                  val maybe: Either[Rejection, Tuple1[(File, Option[String], ContentType)]] = for (file <- fileO.toRight(MissingFormFieldRejection("file"))) yield {
                    Tuple1(file)
                  }
                  maybe.fold(reject(_), tprovide(_))
              }))
            }: Directive[Tuple1[(File, Option[String], ContentType)]]).apply(file => complete(handler.uploadFile(UploadFileResponse)(file)))))
          }
        }
        sealed abstract class UploadFileResponse(val statusCode: StatusCode)
        case object UploadFileResponseCreated extends UploadFileResponse(StatusCodes.Created)
        object UploadFileResponse {
          implicit def uploadFileResponseTRM: ToResponseMarshaller[UploadFileResponse] = Marshaller { implicit ec =>
            resp => uploadFileResponseTR(resp)
          }
          implicit def uploadFileResponseTR(value: UploadFileResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r: UploadFileResponseCreated.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
          }
          def apply[T](value: T)(implicit ev: T => UploadFileResponse): UploadFileResponse = ev(value)
          def Created: UploadFileResponse = UploadFileResponseCreated
        }
      }
    """

    genHandler.structure shouldBe handler.structure
    genResource.structure shouldBe resource.structure
  }
}
