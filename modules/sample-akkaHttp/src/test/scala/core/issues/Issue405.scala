package core.issues

import akka.http.scaladsl.model._
import akka.http.scaladsl.server.{RejectionHandler, Route}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.SpanSugar._
import org.scalatest.{EitherValues, FunSuite, Matchers}
import akka.http.scaladsl.model.FormData
import akka.http.scaladsl.server.Directives.complete

import scala.concurrent.Future

/** Changes
  *
  * - Required String form param should accept empty string
  * - Option String for param should accept emtpy string
  */
class Issue405 extends FunSuite with Matchers with EitherValues with ScalaFutures with ScalatestRouteTest {
  override implicit val patienceConfig = PatienceConfig(10 seconds, 1 second)

  implicit val rejectionHandler: RejectionHandler = RejectionHandler.newBuilder().handle { case rej =>
    complete(HttpResponse(StatusCodes.BadRequest, entity = HttpEntity(ContentTypes.`text/plain(UTF-8)`, s"Request failed with $rej")))
  }.result()

  test("Empty string for a required form param should parse as empty string") {
    import issues.issue405.server.akkaHttp.{ Handler, Resource }
    import issues.issue405.server.akkaHttp.definitions._

    val route = Route.seal(Resource.routes(new Handler {
      override def foo(respond: Resource.fooResponse.type)(bar: String, baz: Option[String]): Future[Resource.fooResponse] =
        Future.successful(respond.OK(s"Bar is '$bar'"))
    }))

    /* Pass empty string to required Bar param */
    Post("/v1/Foo", FormData(Map("Bar" -> ""))) ~> route ~> check {
      response.status shouldBe (StatusCodes.OK)
      responseAs[String] shouldBe "\"Bar is ''\""
    }
  }

  test("Empty string for an optional form param should parse as empty string") {
    import issues.issue405.server.akkaHttp.{ Handler, Resource }
    import issues.issue405.server.akkaHttp.definitions._

    val route = Route.seal(Resource.routes(new Handler {
      override def foo(respond: Resource.fooResponse.type)(bar: String, baz: Option[String]): Future[Resource.fooResponse] = {
        val msg = baz.map(s => s"present: '$s'").getOrElse("missing")
        Future.successful(respond.OK(s"Baz is $msg"))
      }
    }))

    /* Pass empty string to required Bar param */
    Post("/v1/Foo", FormData(Map("Bar" -> "whatevs", "Baz" -> ""))) ~> route ~> check {
      response.status shouldBe (StatusCodes.OK)
      responseAs[String] shouldBe "\"Baz is present: ''\""
    }
  }
}
