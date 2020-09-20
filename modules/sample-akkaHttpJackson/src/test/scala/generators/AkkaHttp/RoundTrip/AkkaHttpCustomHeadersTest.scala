package generators.AkkaHttp.RoundTrip

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import core.TestImplicits
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.SpanSugar._
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.ExecutionContext.Implicits.global
import tests.customTypes.customHeader.client.akkaHttpJackson.Client
import tests.customTypes.customHeader.client.akkaHttpJackson.{ definitions => cdefs }
import tests.customTypes.customHeader.server.akkaHttpJackson.Implicits.Formatter
import tests.customTypes.customHeader.server.akkaHttpJackson.{ Handler, Resource, definitions => sdefs }
import scala.concurrent.Future

class AkkaHttpCustomHeadersTest extends AnyFlatSpec with TestImplicits with Matchers with ScalaFutures with EitherValues {

  override implicit val patienceConfig = PatienceConfig(10 seconds, 1 second)

  it should "encode custom headers" in {
    Formatter.show(sdefs.Bar.V1) shouldBe "v1"
    Formatter.show(sdefs.Bar.ILikeSpaces) shouldBe "i like spaces"
  }

  it should "round-trip encoded values" in {
    implicit val as = ActorSystem()
    val client = Client.httpClient(Route.toFunction(Resource.routes(new Handler {
      def getFoo(respond: Resource.GetFooResponse.type)(
          header: String,
          longHeader: Long,
          customHeader: sdefs.Bar,
          customOptionHeader: Option[sdefs.Bar],
          missingCustomOptionHeader: Option[sdefs.Bar]
      ): Future[Resource.GetFooResponse] =
        (header, longHeader, customHeader, customOptionHeader, missingCustomOptionHeader) match {
          case ("foo", 5L, sdefs.Bar.V1, Some(sdefs.Bar.V2), None) => Future.successful(respond.OK)
          case _                                                   => Future.successful(respond.BadRequest)
        }
    })))

    client.getFoo("foo", 5L, cdefs.Bar.V1, Some(cdefs.Bar.V2), None).value.futureValue.right.value
  }

}
