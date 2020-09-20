package core.AkkaHttp

import _root_.examples.client.akkaHttp.AkkaHttpImplicits.IgnoredEntity
import _root_.examples.client.akkaHttp.pet.{
  AddPetResponse,
  DeletePetResponse,
  FindPetsByStatusEnumResponse,
  FindPetsByStatusResponse,
  PetClient,
  UploadFileResponse
}
import _root_.examples.client.akkaHttp.{ definitions => cdefs }
import _root_.examples.server.akkaHttp.pet.{ PetHandler, PetResource }
import _root_.examples.server.akkaHttp.{ definitions => sdefs }
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import cats.instances.future._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.SpanSugar._
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.Future
import examples.support.PositiveLong

class AkkaHttpRoundTripTest extends AnyFunSuite with Matchers with EitherValues with ScalaFutures with ScalatestRouteTest {
  override implicit val patienceConfig = PatienceConfig(10 seconds, 1 second)

  // Placeholder until property testing
  val id: Option[Long]             = None
  val categoryId: Option[Long]     = None
  val categoryName: Option[String] = None
  val name: String                 = ""
  val photoUrls: Vector[String]    = Vector.empty
  val tag1id: Option[Long]         = None
  val tag1name: Option[String]     = None
  val tag2id: Option[Long]         = None
  val tag2name: Option[String]     = None
  val tag3id: Option[Long]         = None
  val tag3name: Option[String]     = None
  val petStatus: Option[String]    = Some("pending")

  test("round-trip: definition query, unit response") {
    val httpClient = Route.toFunction(PetResource.routes(new PetHandler {
      def addPet(respond: PetResource.AddPetResponse.type)(body: sdefs.Pet): Future[PetResource.AddPetResponse] =
        body match {
          case sdefs.Pet(
              `id`,
              Some(sdefs.Category(`categoryId`, `categoryName`)),
              `name`,
              `photoUrls`,
              None,
              Some(sdefs.PetStatus.Pending)
              ) =>
            Future.successful(respond.Created)
          case _ => failTest("Parameters didn't match")
        }
      def deletePet(
          respond: PetResource.DeletePetResponse.type
      )(_petId: Long, includeChildren: Option[Boolean], status: Option[sdefs.PetStatus], _apiKey: Option[String] = None)                                  = ???
      def findPetsByStatus(respond: PetResource.FindPetsByStatusResponse.type)(status: Iterable[String])                                                  = ???
      def findPetsByStatusEnum(respond: PetResource.FindPetsByStatusEnumResponse.type)(status: sdefs.PetStatus)                                           = ???
      def findPetsByTags(respond: PetResource.FindPetsByTagsResponse.type)(tags: Iterable[String])                                                        = ???
      def getPetById(respond: PetResource.GetPetByIdResponse.type)(petId: Long)                                                                           = ???
      def updatePet(respond: PetResource.UpdatePetResponse.type)(body: sdefs.Pet)                                                                         = ???
      def updatePetWithForm(respond: PetResource.UpdatePetWithFormResponse.type)(petId: Long, name: Option[String] = None, status: Option[String] = None) = ???
      def uploadFileMapFileField(fieldName: String, fileName: Option[String], contentType: ContentType) =
        java.io.File.createTempFile("download_", ".dat", new java.io.File("/tmp"))
      def uploadFile(respond: PetResource.UploadFileResponse.type)(
          petId: examples.support.PositiveLong,
          additionalMetadata: Option[String],
          file: Option[(java.io.File, Option[String], akka.http.scaladsl.model.ContentType)],
          file2: (java.io.File, Option[String], akka.http.scaladsl.model.ContentType),
          file3: (java.io.File, Option[String], akka.http.scaladsl.model.ContentType, String),
          longValue: Long,
          customValue: examples.support.PositiveLong,
          customOptionalValue: Option[examples.support.PositiveLong]
      ) = ???
    }))

    val petClient = PetClient.httpClient(httpClient)

    petClient
      .addPet(
        cdefs.Pet(
          id = id,
          category = Some(cdefs.Category(categoryId, categoryName)),
          name = name,
          photoUrls = photoUrls,
          tags = None,
          status = Some(cdefs.PetStatus.Pending)
        )
      )
      .value
      .futureValue should be(Right(AddPetResponse.Created))
  }

  test("round-trip: enum query, Vector of definition response") {
    val httpClient = Route.toFunction(PetResource.routes(new PetHandler {
      def findPetsByStatusEnum(
          respond: PetResource.FindPetsByStatusEnumResponse.type
      )(_status: sdefs.PetStatus): Future[PetResource.FindPetsByStatusEnumResponse] =
        Future.successful(petStatus.fold(Vector.empty[sdefs.Pet])({ value =>
          if (_status.value == value) {
            Vector(
              sdefs.Pet(
                id = id,
                category = Some(sdefs.Category(categoryId, categoryName)),
                name = name,
                photoUrls = photoUrls,
                tags = None,
                status = sdefs.PetStatus.parse(value)
              )
            )
          } else {
            failTest("Parameters didn't match!")
          }
        }))

      def addPet(respond: PetResource.AddPetResponse.type)(body: sdefs.Pet) = ???
      def deletePet(
          respond: PetResource.DeletePetResponse.type
      )(_petId: Long, includeChildren: Option[Boolean], status: Option[sdefs.PetStatus], _apiKey: Option[String] = None)                                  = ???
      def findPetsByStatus(respond: PetResource.FindPetsByStatusResponse.type)(status: Iterable[String])                                                  = ???
      def findPetsByTags(respond: PetResource.FindPetsByTagsResponse.type)(tags: Iterable[String])                                                        = ???
      def getPetById(respond: PetResource.GetPetByIdResponse.type)(petId: Long)                                                                           = ???
      def updatePet(respond: PetResource.UpdatePetResponse.type)(body: sdefs.Pet)                                                                         = ???
      def updatePetWithForm(respond: PetResource.UpdatePetWithFormResponse.type)(petId: Long, name: Option[String] = None, status: Option[String] = None) = ???
      def uploadFileMapFileField(fieldName: String, fileName: Option[String], contentType: ContentType) =
        java.io.File.createTempFile("download_", ".dat", new java.io.File("/tmp"))
      def uploadFile(respond: PetResource.UploadFileResponse.type)(
          petId: examples.support.PositiveLong,
          additionalMetadata: Option[String],
          file: Option[(java.io.File, Option[String], akka.http.scaladsl.model.ContentType)],
          file2: (java.io.File, Option[String], akka.http.scaladsl.model.ContentType),
          file3: (java.io.File, Option[String], akka.http.scaladsl.model.ContentType, String),
          longValue: Long,
          customValue: examples.support.PositiveLong,
          customOptionalValue: Option[examples.support.PositiveLong]
      ) = ???
    }))

    val petClient = PetClient.httpClient(httpClient)

    petClient.findPetsByStatusEnum(cdefs.PetStatus.Pending).value.futureValue should be(
      Right(
        FindPetsByStatusEnumResponse.OK(
          Vector(
            cdefs.Pet(
              id = id,
              category = Some(cdefs.Category(categoryId, categoryName)),
              name = name,
              photoUrls = photoUrls,
              tags = None,
              status = Some(cdefs.PetStatus.Pending)
            )
          )
        )
      )
    )
  }

  test("round-trip: 404 response") {
    val httpClient = Route.toFunction(PetResource.routes(new PetHandler {
      def findPetsByStatus(respond: PetResource.FindPetsByStatusResponse.type)(status: Iterable[String]): Future[PetResource.FindPetsByStatusResponse] =
        Future.successful(respond.NotFound)

      def addPet(respond: PetResource.AddPetResponse.type)(body: sdefs.Pet) = ???
      def deletePet(
          respond: PetResource.DeletePetResponse.type
      )(_petId: Long, includeChildren: Option[Boolean], status: Option[sdefs.PetStatus], _apiKey: Option[String] = None)                                  = ???
      def findPetsByStatusEnum(respond: PetResource.FindPetsByStatusEnumResponse.type)(status: sdefs.PetStatus)                                           = ???
      def findPetsByTags(respond: PetResource.FindPetsByTagsResponse.type)(tags: Iterable[String])                                                        = ???
      def getPetById(respond: PetResource.GetPetByIdResponse.type)(petId: Long)                                                                           = ???
      def updatePet(respond: PetResource.UpdatePetResponse.type)(body: sdefs.Pet)                                                                         = ???
      def updatePetWithForm(respond: PetResource.UpdatePetWithFormResponse.type)(petId: Long, name: Option[String] = None, status: Option[String] = None) = ???
      def uploadFileMapFileField(fieldName: String, fileName: Option[String], contentType: ContentType) =
        java.io.File.createTempFile("download_", ".dat", new java.io.File("/tmp"))
      def uploadFile(respond: PetResource.UploadFileResponse.type)(
          petId: examples.support.PositiveLong,
          additionalMetadata: Option[String],
          file: Option[(java.io.File, Option[String], akka.http.scaladsl.model.ContentType)],
          file2: (java.io.File, Option[String], akka.http.scaladsl.model.ContentType),
          file3: (java.io.File, Option[String], akka.http.scaladsl.model.ContentType, String),
          longValue: Long,
          customValue: examples.support.PositiveLong,
          customOptionalValue: Option[examples.support.PositiveLong]
      ) = ???
    }))

    val petClient = PetClient.httpClient(httpClient)

    petClient.findPetsByStatus(Vector("bogus")).value.futureValue should be(
      Right(
        FindPetsByStatusResponse.NotFound
      )
    )
  }

  test("round-trip: Raw type parameters") {
    val petId: Long    = 123L
    val apiKey: String = "foobar"
    val httpClient = Route.toFunction(PetResource.routes(new PetHandler {
      def deletePet(respond: PetResource.DeletePetResponse.type)(
          _petId: Long,
          includeChildren: Option[Boolean],
          status: Option[sdefs.PetStatus],
          _apiKey: Option[String] = None
      ): Future[PetResource.DeletePetResponse] =
        if (_petId == petId && _apiKey.contains(apiKey) && status.contains(sdefs.PetStatus.Pending))
          Future.successful(respond.OK)
        else Future.successful(respond.NotFound)

      def addPet(respond: PetResource.AddPetResponse.type)(body: sdefs.Pet)                                                                               = ???
      def findPetsByStatus(respond: PetResource.FindPetsByStatusResponse.type)(status: Iterable[String])                                                  = ???
      def findPetsByStatusEnum(respond: PetResource.FindPetsByStatusEnumResponse.type)(status: sdefs.PetStatus)                                           = ???
      def findPetsByTags(respond: PetResource.FindPetsByTagsResponse.type)(tags: Iterable[String])                                                        = ???
      def getPetById(respond: PetResource.GetPetByIdResponse.type)(petId: Long)                                                                           = ???
      def updatePet(respond: PetResource.UpdatePetResponse.type)(body: sdefs.Pet)                                                                         = ???
      def updatePetWithForm(respond: PetResource.UpdatePetWithFormResponse.type)(petId: Long, name: Option[String] = None, status: Option[String] = None) = ???
      def uploadFileMapFileField(fieldName: String, fileName: Option[String], contentType: ContentType) =
        java.io.File.createTempFile("download_", ".dat", new java.io.File("/tmp"))
      def uploadFile(respond: PetResource.UploadFileResponse.type)(
          petId: examples.support.PositiveLong,
          additionalMetadata: Option[String],
          file: Option[(java.io.File, Option[String], akka.http.scaladsl.model.ContentType)],
          file2: (java.io.File, Option[String], akka.http.scaladsl.model.ContentType),
          file3: (java.io.File, Option[String], akka.http.scaladsl.model.ContentType, String),
          longValue: Long,
          customValue: examples.support.PositiveLong,
          customOptionalValue: Option[examples.support.PositiveLong]
      ) = ???
    }))

    val petClient = PetClient.httpClient(httpClient)

    val result = petClient.deletePet(petId, Some(true), Some(cdefs.PetStatus.Pending), Some(apiKey)).value.futureValue
    result
      .fold({ err =>
        failTest(err.toString)
      }, {
        case DeletePetResponse.OK         => ()
        case DeletePetResponse.BadRequest => ()
        case DeletePetResponse.NotFound   => ()
      })
  }

  test("round-trip: File uploads") {
    val petId: Long    = 123L
    val apiKey: String = "foobar"
    val httpClient = Route.toFunction(PetResource.routes(new PetHandler {
      def addPet(respond: PetResource.AddPetResponse.type)(body: sdefs.Pet) = ???
      def deletePet(
          respond: PetResource.DeletePetResponse.type
      )(_petId: Long, includeChildren: Option[Boolean], status: Option[sdefs.PetStatus], _apiKey: Option[String] = None)                                  = ???
      def findPetsByStatus(respond: PetResource.FindPetsByStatusResponse.type)(status: Iterable[String])                                                  = ???
      def findPetsByStatusEnum(respond: PetResource.FindPetsByStatusEnumResponse.type)(status: sdefs.PetStatus)                                           = ???
      def findPetsByTags(respond: PetResource.FindPetsByTagsResponse.type)(tags: Iterable[String])                                                        = ???
      def getPetById(respond: PetResource.GetPetByIdResponse.type)(petId: Long)                                                                           = ???
      def updatePet(respond: PetResource.UpdatePetResponse.type)(body: sdefs.Pet)                                                                         = ???
      def updatePetWithForm(respond: PetResource.UpdatePetWithFormResponse.type)(petId: Long, name: Option[String] = None, status: Option[String] = None) = ???
      def uploadFileMapFileField(fieldName: String, fileName: Option[String], contentType: ContentType) =
        java.io.File.createTempFile("download_", ".dat", new java.io.File("/tmp"))
      def uploadFile(respond: PetResource.UploadFileResponse.type)(
          petId: examples.support.PositiveLong,
          additionalMetadata: Option[String],
          file: Option[(java.io.File, Option[String], akka.http.scaladsl.model.ContentType)],
          file2: (java.io.File, Option[String], akka.http.scaladsl.model.ContentType),
          file3: (java.io.File, Option[String], akka.http.scaladsl.model.ContentType, String),
          longValue: Long,
          customValue: examples.support.PositiveLong,
          customOptionalValue: Option[examples.support.PositiveLong]
      ) = {
        val f1Length = file.flatMap({
          case (f, _, _) =>
            if (f.exists) {
              Some(f.length)
            } else None
        })
        val f2Length = if (file2._1.exists) {
          Some(file2._1.length)
        } else None
        val f3Length = if (file3._1.exists) {
          Some(file3._1.length)
        } else None

        assert(file3._4 == "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855", "Empty file hash does not match")

        val code = file.count(_._1.exists) + (if (file2._1.exists) 1 else 0) + (if (file3._1.exists) 1 else 0)

        Future.successful(respond.OK(sdefs.ApiResponse(code = Some(code), message = Some(s"${f1Length} ${f2Length} ${f3Length}"))))
      }
    }))

    val petClient = PetClient.httpClient(httpClient)

    val result = petClient
      .uploadFile(
        PositiveLong(petId).get,
        Some("Additional metadata"),
        None,
        HttpEntity(ContentTypes.`application/json`, ""),
        HttpEntity(ContentTypes.`application/json`, ""),
        5L,
        PositiveLong(10L).get,
        PositiveLong(20L)
      )
      .value
      .futureValue
    result
      .fold({ err =>
        failTest(err.toString)
      }, {
        case UploadFileResponse.OK(resp) => assert(resp.code.exists(_ == 2), "Unexpected number of file uploads!")
      })
  }
}
