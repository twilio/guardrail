package core.AkkaHttpJackson

import _root_.tracer.client.akkaHttpJackson.addresses.{ AddressesClient, GetAddressResponse }
import _root_.tracer.client.akkaHttpJackson.users.{ GetUserResponse, UsersClient }
import _root_.tracer.client.akkaHttpJackson.{ definitions => cdefs }
import _root_.tracer.server.akkaHttpJackson.AkkaHttpImplicits.TraceBuilder
import _root_.tracer.server.akkaHttpJackson.addresses.{ AddressesHandler, AddressesResource }
import _root_.tracer.server.akkaHttpJackson.users.{ UsersHandler, UsersResource }
import _root_.tracer.server.akkaHttpJackson.{ definitions => sdefs }
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import cats.instances.future._
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import core.TestImplicits
import org.scalatest.EitherValues
import org.scalatest.concurrent.{ IntegrationPatience, ScalaFutures }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.concurrent.{ ExecutionContext, Future }

class AkkaHttpFullTracerTest
    extends AnyFunSuite
    with TestImplicits
    with Matchers
    with EitherValues
    with ScalaFutures
    with ScalatestRouteTest
    with IntegrationPatience {

  val traceHeaderKey          = "tracer-label"
  def log(line: String): Unit = ()

  def trace(implicit ec: ExecutionContext): String => Directive1[TraceBuilder] = { name =>
    // In a real environment, this would be where you could establish a new
    // tracing context and inject that fresh header value.
    log(s"Expecting all requests to have ${traceHeaderKey} header, otherwise 400.")
    for {
      traceValue <- headerValueByName(traceHeaderKey)
    } yield {
      traceBuilder(traceValue)
    }
  }

  def traceBuilder(parentValue: String)(implicit ec: ExecutionContext): TraceBuilder = {
    name => httpClient =>
      { req =>
        // Rudimentary testing. As we have the response object in res, we could
        // also log error codes or other interesting metrics.
        val before = System.currentTimeMillis
        for {
          res <- httpClient(req.mapHeaders(RawHeader(traceHeaderKey, parentValue) +: _))
          after = System.currentTimeMillis
          ()    = log(s"Request took ${after - before}ms")
        } yield res
      }
  }

  test("full tracer: passing headers through multiple levels") {
    // Establish the "Address" server
    val server2: HttpRequest => Future[HttpResponse] = Route.toFunction(
      AddressesResource.routes(
        new AddressesHandler {
          def getAddress(respond: AddressesResource.GetAddressResponse.type)(id: String)(traceBuilder: TraceBuilder) =
            Future.successful(if (id == "addressId") {
              respond.OK(sdefs.Address(Some("line1"), Some("line2"), Some("line3")))
            } else respond.NotFound)
          def getAddresses(respond: AddressesResource.GetAddressesResponse.type)()(traceBuilder: TraceBuilder) =
            Future.successful(respond.NotFound)
        },
        trace
      )
    )

    // Establish the "User" server
    val server1: HttpRequest => Future[HttpResponse] = Route.toFunction(
      UsersResource.routes(
        new UsersHandler {
          // ... using the "Address" server explicitly in the addressesClient
          val addressesClient = AddressesClient.httpClient(server2)
          def getUser(respond: UsersResource.GetUserResponse.type)(id: String)(traceBuilder: TraceBuilder) =
            addressesClient
              .getAddress(traceBuilder, "addressId")
              .fold(
                _ => respond.NotFound, {
                  case GetAddressResponse.OK(address) => respond.OK(sdefs.User("1234", sdefs.UserAddress(address.line1, address.line2, address.line3)))
                  case GetAddressResponse.NotFound    => respond.NotFound
                }
              )
        },
        trace
      )
    )

    // Build a UsersClient using the User server
    val usersClient = UsersClient.httpClient(server1)
    // As this is the entry point, we either have a tracing header from
    // somewhere else, or we generate one for top-level request.
    val testTrace = traceBuilder("top-level-request")

    // Make a request against the mock servers using a hard-coded user ID
    val retrieved: GetUserResponse = usersClient.getUser(testTrace, "1234").value.futureValue.right.value

    retrieved shouldBe (GetUserResponse.OK(cdefs.User("1234", cdefs.UserAddress(Some("line1"), Some("line2"), Some("line3")))))
  }
}
