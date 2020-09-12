package core.issues

import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import cats.implicits._
import org.scalactic.source
import org.scalatest.EitherValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.concurrent.Future
import scala.concurrent.duration._

class Issue315Suite extends AnyFunSuite with Matchers with EitherValues with ScalaFutures with ScalatestRouteTest {
  override implicit val patienceConfig = PatienceConfig(1000.millis, 1000.millis)

  def expectSuccess[A, B](value: Either[Either[Throwable, A], B])(implicit pos: source.Position): Unit = {
    val _ = value.right.value
  }

  def expectProtocolError[A, B](value: Either[Either[Throwable, A], B])(implicit pos: source.Position): Unit = {
    val _ = value.left.value.right.value
  }

  object client {
    def legacylegacy(httpClient: HttpRequest => Future[HttpResponse]) = {
      import _root_.issues.issue315.legacylegacy.client.akkaHttp.Client
      import _root_.issues.issue315.legacylegacy.client.akkaHttp.definitions._
      import _root_.issues.issue315.legacylegacy.client.akkaHttp.support.Presence
      val clientBody = TestObject(
        required = "foo",
        requiredNullable = None,
        optional = Presence.absent[String],
        optionalNullable = Presence.absent[Option[String]],
        legacy = None
      )
      Client
        .httpClient(httpClient)
        .postTest(clientBody)
        .map(_.fold(succeed, fail("Server business logic rejected request")))
        .value
        .futureValue
    }

    def legacyoptional(httpClient: HttpRequest => Future[HttpResponse]) = {
      import _root_.issues.issue315.legacyoptional.client.akkaHttp.Client
      import _root_.issues.issue315.legacyoptional.client.akkaHttp.definitions._
      import _root_.issues.issue315.legacyoptional.client.akkaHttp.support.Presence
      val clientBody = TestObject(
        required = "foo",
        requiredNullable = None,
        optional = Presence.absent[String],
        optionalNullable = Presence.absent[Option[String]],
        legacy = None
      )
      Client
        .httpClient(httpClient)
        .postTest(clientBody)
        .map(_.fold(succeed, fail("Server business logic rejected request")))
        .value
        .futureValue
    }

    def legacyrequiredNullable(httpClient: HttpRequest => Future[HttpResponse]) = {
      import _root_.issues.issue315.legacyrequiredNullable.client.akkaHttp.Client
      import _root_.issues.issue315.legacyrequiredNullable.client.akkaHttp.definitions._
      import _root_.issues.issue315.legacyrequiredNullable.client.akkaHttp.support.Presence
      val clientBody = TestObject(
        required = "foo",
        requiredNullable = None,
        optional = Presence.absent[String],
        optionalNullable = Presence.absent[Option[String]],
        legacy = None
      )
      Client
        .httpClient(httpClient)
        .postTest(clientBody)
        .map(_.fold(succeed, fail("Server business logic rejected request")))
        .value
        .futureValue
    }

    def optionallegacy(httpClient: HttpRequest => Future[HttpResponse]) = {
      import _root_.issues.issue315.optionallegacy.client.akkaHttp.Client
      import _root_.issues.issue315.optionallegacy.client.akkaHttp.definitions._
      import _root_.issues.issue315.optionallegacy.client.akkaHttp.support.Presence
      val clientBody = TestObject(
        required = "foo",
        requiredNullable = None,
        optional = Presence.absent[String],
        optionalNullable = Presence.absent[Option[String]],
        legacy = None
      )
      Client
        .httpClient(httpClient)
        .postTest(clientBody)
        .map(_.fold(succeed, fail("Server business logic rejected request")))
        .value
        .futureValue
    }

    def optionaloptional(httpClient: HttpRequest => Future[HttpResponse]) = {
      import _root_.issues.issue315.optionaloptional.client.akkaHttp.Client
      import _root_.issues.issue315.optionaloptional.client.akkaHttp.definitions._
      import _root_.issues.issue315.optionaloptional.client.akkaHttp.support.Presence
      val clientBody = TestObject(
        required = "foo",
        requiredNullable = None,
        optional = Presence.absent[String],
        optionalNullable = Presence.absent[Option[String]],
        legacy = Presence.absent
      )
      Client
        .httpClient(httpClient)
        .postTest(clientBody)
        .map(_.fold(succeed, fail("Server business logic rejected request")))
        .value
        .futureValue
    }

    def optionalrequiredNullable(httpClient: HttpRequest => Future[HttpResponse]) = {
      import _root_.issues.issue315.optionalrequiredNullable.client.akkaHttp.Client
      import _root_.issues.issue315.optionalrequiredNullable.client.akkaHttp.definitions._
      import _root_.issues.issue315.optionalrequiredNullable.client.akkaHttp.support.Presence
      val clientBody = TestObject(
        required = "foo",
        requiredNullable = None,
        optional = Presence.absent[String],
        optionalNullable = Presence.absent[Option[String]],
        legacy = None
      )
      Client
        .httpClient(httpClient)
        .postTest(clientBody)
        .map(_.fold(succeed, fail("Server business logic rejected request")))
        .value
        .futureValue
    }

    def requiredNullablelegacy(httpClient: HttpRequest => Future[HttpResponse]) = {
      import _root_.issues.issue315.requiredNullablelegacy.client.akkaHttp.Client
      import _root_.issues.issue315.requiredNullablelegacy.client.akkaHttp.definitions._
      import _root_.issues.issue315.requiredNullablelegacy.client.akkaHttp.support.Presence
      val clientBody = TestObject(
        required = "foo",
        requiredNullable = None,
        optional = Presence.absent[String],
        optionalNullable = Presence.absent[Option[String]],
        legacy = None
      )
      Client
        .httpClient(httpClient)
        .postTest(clientBody)
        .map(_.fold(succeed, fail("Server business logic rejected request")))
        .value
        .futureValue
    }

    def requiredNullableoptional(httpClient: HttpRequest => Future[HttpResponse]) = {
      import _root_.issues.issue315.requiredNullableoptional.client.akkaHttp.Client
      import _root_.issues.issue315.requiredNullableoptional.client.akkaHttp.definitions._
      import _root_.issues.issue315.requiredNullableoptional.client.akkaHttp.support.Presence
      val clientBody = TestObject(
        required = "foo",
        requiredNullable = None,
        optional = Presence.absent[String],
        optionalNullable = Presence.absent[Option[String]],
        legacy = None
      )
      Client
        .httpClient(httpClient)
        .postTest(clientBody)
        .map(_.fold(succeed, fail("Server business logic rejected request")))
        .value
        .futureValue
    }

    def requiredNullablerequiredNullable(httpClient: HttpRequest => Future[HttpResponse]) = {
      import _root_.issues.issue315.requiredNullablerequiredNullable.client.akkaHttp.Client
      import _root_.issues.issue315.requiredNullablerequiredNullable.client.akkaHttp.definitions._
      import _root_.issues.issue315.requiredNullablerequiredNullable.client.akkaHttp.support.Presence
      val clientBody = TestObject(
        required = "foo",
        requiredNullable = None,
        optional = Presence.absent[String],
        optionalNullable = Presence.absent[Option[String]],
        legacy = None
      )
      Client
        .httpClient(httpClient)
        .postTest(clientBody)
        .map(_.fold(succeed, fail("Server business logic rejected request")))
        .value
        .futureValue
    }
  }

  object server {
    val legacylegacy: HttpRequest => Future[HttpResponse] = {
      import _root_.issues.issue315.legacylegacy.server.akkaHttp.definitions._
      import _root_.issues.issue315.legacylegacy.server.akkaHttp.support.Presence
      import _root_.issues.issue315.legacylegacy.server.akkaHttp.{ Handler, Resource }
      Route.toFunction(Resource.routes(new Handler {
        def postTest(respond: Resource.PostTestResponse.type)(body: TestObject) =
          body match {
            case TestObject("foo", None, Presence.Absent, Presence.Absent, None) => respond.OK.pure[Future]
            case _                                                               => respond.NotAcceptable.pure[Future]
          }
      }))
    }

    val legacyoptional: HttpRequest => Future[HttpResponse] = {
      import _root_.issues.issue315.legacyoptional.server.akkaHttp.definitions._
      import _root_.issues.issue315.legacyoptional.server.akkaHttp.support.Presence
      import _root_.issues.issue315.legacyoptional.server.akkaHttp.{ Handler, Resource }
      Route.toFunction(Resource.routes(new Handler {
        def postTest(respond: Resource.PostTestResponse.type)(body: TestObject) =
          body match {
            case TestObject("foo", None, Presence.Absent, Presence.Absent, None) => respond.OK.pure[Future]
            case _                                                               => respond.NotAcceptable.pure[Future]
          }
      }))
    }

    val legacyrequiredNullable: HttpRequest => Future[HttpResponse] = {
      import _root_.issues.issue315.legacyrequiredNullable.server.akkaHttp.definitions._
      import _root_.issues.issue315.legacyrequiredNullable.server.akkaHttp.support.Presence
      import _root_.issues.issue315.legacyrequiredNullable.server.akkaHttp.{ Handler, Resource }
      Route.toFunction(Resource.routes(new Handler {
        def postTest(respond: Resource.PostTestResponse.type)(body: TestObject) =
          body match {
            case TestObject("foo", None, Presence.Absent, Presence.Absent, None) => respond.OK.pure[Future]
            case _                                                               => respond.NotAcceptable.pure[Future]
          }
      }))
    }

    val optionallegacy: HttpRequest => Future[HttpResponse] = {
      import _root_.issues.issue315.optionallegacy.server.akkaHttp.definitions._
      import _root_.issues.issue315.optionallegacy.server.akkaHttp.support.Presence
      import _root_.issues.issue315.optionallegacy.server.akkaHttp.{ Handler, Resource }
      Route.toFunction(Resource.routes(new Handler {
        def postTest(respond: Resource.PostTestResponse.type)(body: TestObject) =
          body match {
            case TestObject("foo", None, Presence.Absent, Presence.Absent, None) => respond.OK.pure[Future]
            case _                                                               => respond.NotAcceptable.pure[Future]
          }
      }))
    }

    val optionaloptional: HttpRequest => Future[HttpResponse] = {
      import _root_.issues.issue315.optionaloptional.server.akkaHttp.definitions._
      import _root_.issues.issue315.optionaloptional.server.akkaHttp.support.Presence
      import _root_.issues.issue315.optionaloptional.server.akkaHttp.{ Handler, Resource }
      Route.toFunction(Resource.routes(new Handler {
        def postTest(respond: Resource.PostTestResponse.type)(body: TestObject) =
          body match {
            case TestObject("foo", None, Presence.Absent, Presence.Absent, Presence.Absent) => respond.OK.pure[Future]
            case _                                                                          => respond.NotAcceptable.pure[Future]
          }
      }))
    }

    val optionalrequiredNullable: HttpRequest => Future[HttpResponse] = {
      import _root_.issues.issue315.optionalrequiredNullable.server.akkaHttp.definitions._
      import _root_.issues.issue315.optionalrequiredNullable.server.akkaHttp.support.Presence
      import _root_.issues.issue315.optionalrequiredNullable.server.akkaHttp.{ Handler, Resource }
      Route.toFunction(Resource.routes(new Handler {
        def postTest(respond: Resource.PostTestResponse.type)(body: TestObject) =
          body match {
            case TestObject("foo", None, Presence.Absent, Presence.Absent, None) => respond.OK.pure[Future]
            case _                                                               => respond.NotAcceptable.pure[Future]
          }
      }))
    }

    val requiredNullablelegacy: HttpRequest => Future[HttpResponse] = {
      import _root_.issues.issue315.requiredNullablelegacy.server.akkaHttp.definitions._
      import _root_.issues.issue315.requiredNullablelegacy.server.akkaHttp.support.Presence
      import _root_.issues.issue315.requiredNullablelegacy.server.akkaHttp.{ Handler, Resource }
      Route.toFunction(Resource.routes(new Handler {
        def postTest(respond: Resource.PostTestResponse.type)(body: TestObject) =
          body match {
            case TestObject("foo", None, Presence.Absent, Presence.Absent, None) => respond.OK.pure[Future]
            case _                                                               => respond.NotAcceptable.pure[Future]
          }
      }))
    }

    val requiredNullableoptional: HttpRequest => Future[HttpResponse] = {
      import _root_.issues.issue315.requiredNullableoptional.server.akkaHttp.definitions._
      import _root_.issues.issue315.requiredNullableoptional.server.akkaHttp.support.Presence
      import _root_.issues.issue315.requiredNullableoptional.server.akkaHttp.{ Handler, Resource }
      Route.toFunction(Resource.routes(new Handler {
        def postTest(respond: Resource.PostTestResponse.type)(body: TestObject) =
          body match {
            case TestObject("foo", None, Presence.Absent, Presence.Absent, None) => respond.OK.pure[Future]
            case _                                                               => respond.NotAcceptable.pure[Future]
          }
      }))
    }

    val requiredNullablerequiredNullable: HttpRequest => Future[HttpResponse] = {
      import _root_.issues.issue315.requiredNullablerequiredNullable.server.akkaHttp.definitions._
      import _root_.issues.issue315.requiredNullablerequiredNullable.server.akkaHttp.support.Presence
      import _root_.issues.issue315.requiredNullablerequiredNullable.server.akkaHttp.{ Handler, Resource }
      Route.toFunction(Resource.routes(new Handler {
        def postTest(respond: Resource.PostTestResponse.type)(body: TestObject) =
          body match {
            case TestObject("foo", None, Presence.Absent, Presence.Absent, None) => respond.OK.pure[Future]
            case _                                                               => respond.NotAcceptable.pure[Future]
          }
      }))
    }
  }

  test("expected protocol rejections") {
    // `"legacy": null` (from required/nullable) incompatible with `optional` decoder
    expectProtocolError(client.requiredNullableoptional(server.legacyoptional))
    expectProtocolError(client.requiredNullableoptional(server.optionaloptional))
    expectProtocolError(client.requiredNullableoptional(server.requiredNullableoptional))

    // `"legacy": null` (from legacy encoder) incompatible with `optional` decoder
    expectProtocolError(client.legacylegacy(server.legacyoptional))
    expectProtocolError(client.legacylegacy(server.optionaloptional))
    expectProtocolError(client.legacylegacy(server.requiredNullableoptional))
    expectProtocolError(client.legacyoptional(server.legacyoptional))
    expectProtocolError(client.legacyoptional(server.optionaloptional))
    expectProtocolError(client.legacyoptional(server.requiredNullableoptional))
    expectProtocolError(client.legacyrequiredNullable(server.legacyoptional))
    expectProtocolError(client.legacyrequiredNullable(server.optionaloptional))
    expectProtocolError(client.legacyrequiredNullable(server.requiredNullableoptional))

    // `"legacy": null` incompatible with `optional` decoder
    expectProtocolError(client.requiredNullablelegacy(server.legacyoptional))
    expectProtocolError(client.requiredNullablelegacy(server.optionaloptional))
    expectProtocolError(client.requiredNullablelegacy(server.requiredNullableoptional))
    expectProtocolError(client.requiredNullablerequiredNullable(server.legacyoptional))
    expectProtocolError(client.requiredNullablerequiredNullable(server.optionaloptional))
    expectProtocolError(client.requiredNullablerequiredNullable(server.requiredNullableoptional))

    // Missing `legacy` incompatible with `requiredNullable`
    expectProtocolError(client.optionallegacy(server.legacyrequiredNullable))
    expectProtocolError(client.optionallegacy(server.optionalrequiredNullable))
    expectProtocolError(client.optionallegacy(server.requiredNullablerequiredNullable))
    expectProtocolError(client.optionaloptional(server.legacyrequiredNullable))
    expectProtocolError(client.optionaloptional(server.optionalrequiredNullable))
    expectProtocolError(client.optionaloptional(server.requiredNullablerequiredNullable))
    expectProtocolError(client.optionalrequiredNullable(server.legacyrequiredNullable))
    expectProtocolError(client.optionalrequiredNullable(server.optionalrequiredNullable))
    expectProtocolError(client.optionalrequiredNullable(server.requiredNullablerequiredNullable))
  }

  test("expect success") {
    expectSuccess(client.legacylegacy(server.legacylegacy))
    expectSuccess(client.legacylegacy(server.legacyrequiredNullable))
    expectSuccess(client.legacylegacy(server.optionallegacy))
    expectSuccess(client.legacylegacy(server.optionalrequiredNullable))
    expectSuccess(client.legacylegacy(server.requiredNullablelegacy))
    expectSuccess(client.legacylegacy(server.requiredNullablerequiredNullable))
    expectSuccess(client.legacyoptional(server.legacylegacy))
    expectSuccess(client.legacyoptional(server.legacyrequiredNullable))
    expectSuccess(client.legacyoptional(server.optionallegacy))
    expectSuccess(client.legacyoptional(server.optionalrequiredNullable))
    expectSuccess(client.legacyoptional(server.requiredNullablelegacy))
    expectSuccess(client.legacyoptional(server.requiredNullablerequiredNullable))
    expectSuccess(client.legacyrequiredNullable(server.legacylegacy))
    expectSuccess(client.legacyrequiredNullable(server.legacyrequiredNullable))
    expectSuccess(client.legacyrequiredNullable(server.optionallegacy))
    expectSuccess(client.legacyrequiredNullable(server.optionalrequiredNullable))
    expectSuccess(client.legacyrequiredNullable(server.requiredNullablelegacy))
    expectSuccess(client.legacyrequiredNullable(server.requiredNullablerequiredNullable))
    expectSuccess(client.optionallegacy(server.legacylegacy))
    expectSuccess(client.optionallegacy(server.legacyoptional))
    expectSuccess(client.optionallegacy(server.optionallegacy))
    expectSuccess(client.optionallegacy(server.optionaloptional))
    expectSuccess(client.optionallegacy(server.requiredNullablelegacy))
    expectSuccess(client.optionallegacy(server.requiredNullableoptional))
    expectSuccess(client.optionaloptional(server.legacylegacy))
    expectSuccess(client.optionaloptional(server.legacyoptional))
    expectSuccess(client.optionaloptional(server.optionallegacy))
    expectSuccess(client.optionaloptional(server.optionaloptional))
    expectSuccess(client.optionaloptional(server.requiredNullablelegacy))
    expectSuccess(client.optionaloptional(server.requiredNullableoptional))
    expectSuccess(client.optionalrequiredNullable(server.legacylegacy))
    expectSuccess(client.optionalrequiredNullable(server.legacyoptional))
    expectSuccess(client.optionalrequiredNullable(server.optionallegacy))
    expectSuccess(client.optionalrequiredNullable(server.optionaloptional))
    expectSuccess(client.optionalrequiredNullable(server.requiredNullablelegacy))
    expectSuccess(client.optionalrequiredNullable(server.requiredNullableoptional))
    expectSuccess(client.requiredNullablelegacy(server.legacylegacy))
    expectSuccess(client.requiredNullablelegacy(server.legacyrequiredNullable))
    expectSuccess(client.requiredNullablelegacy(server.optionallegacy))
    expectSuccess(client.requiredNullablelegacy(server.optionalrequiredNullable))
    expectSuccess(client.requiredNullablelegacy(server.requiredNullablelegacy))
    expectSuccess(client.requiredNullablelegacy(server.requiredNullablerequiredNullable))
    expectSuccess(client.requiredNullableoptional(server.legacylegacy))
    expectSuccess(client.requiredNullableoptional(server.legacyrequiredNullable))
    expectSuccess(client.requiredNullableoptional(server.optionallegacy))
    expectSuccess(client.requiredNullableoptional(server.optionalrequiredNullable))
    expectSuccess(client.requiredNullableoptional(server.requiredNullablelegacy))
    expectSuccess(client.requiredNullableoptional(server.requiredNullablerequiredNullable))
    expectSuccess(client.requiredNullablerequiredNullable(server.legacylegacy))
    expectSuccess(client.requiredNullablerequiredNullable(server.legacyrequiredNullable))
    expectSuccess(client.requiredNullablerequiredNullable(server.optionallegacy))
    expectSuccess(client.requiredNullablerequiredNullable(server.optionalrequiredNullable))
    expectSuccess(client.requiredNullablerequiredNullable(server.requiredNullablelegacy))
    expectSuccess(client.requiredNullablerequiredNullable(server.requiredNullablerequiredNullable))
  }
}
