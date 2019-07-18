package core.issues

import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.unmarshalling.Unmarshaller
import cats.instances.future._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.SpanSugar._
import org.scalatest.{ EitherValues, FunSuite, Matchers }
import scala.concurrent.Future
import io.circe._
import _root_.jawn.IncompleteParseException

class IssueXSuite extends FunSuite with Matchers with EitherValues with ScalaFutures with ScalatestRouteTest {
  override implicit val patienceConfig = PatienceConfig(10 seconds, 1 second)

  test("akka-http server can respond when query is an array of integers") {
    import issues.issueX.server.akkaHttp.{ Handler, Resource }
    import issues.issueX.server.akkaHttp.definitions._
    val route = Resource.routes(new Handler {
      override def getFoo(respond: Resource.getFooResponse.type)(keys: Option[Iterable[Int]] = None): Future[Resource.getFooResponse] =
        Future.successful(respond.OK)
    })

    Get("/foo?keys=1&keys=2") ~> route ~> check {
      withClue(responseAs[String]) {
        status should equal(StatusCodes.NoContent)
        response.entity.contentType should equal(ContentTypes.NoContentType)
        response.entity.contentLengthOption should equal(Some(0))
      }
    }
  }
}
