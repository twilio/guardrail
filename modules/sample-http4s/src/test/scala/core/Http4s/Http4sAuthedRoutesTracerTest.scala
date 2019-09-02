package core.Http4s

import _root_.tracer.client.http4s.secure.addresses.AddressesClient
import _root_.tracer.client.{ http4s => cdefs }
import _root_.tracer.server.http4s.Http4sImplicits.TraceBuilder
import _root_.tracer.server.http4s.secure.addresses.{ AddressesHandler, AddressesResource, GetSecureAddressResponse, GetSecureAddressesResponse }
import _root_.tracer.server.{ http4s => sdefs }
import cats.data.{ Kleisli, OptionT }
import cats.effect.IO
import org.http4s.client.{ Client, UnexpectedStatus }
import org.http4s.headers.Authorization
import org.http4s.implicits._
import org.http4s.server.AuthMiddleware
import org.http4s.syntax.StringSyntax
import org.http4s.{ AuthedRoutes, BasicCredentials, Header, Request, Status }
import org.scalatest.{ EitherValues, FunSuite, Matchers }
import tracer.client.http4s.secure.addresses

class Http4sAuthedRoutesTracerTest extends FunSuite with Matchers with EitherValues with StringSyntax {

  val traceHeaderKey          = "tracer-label"
  def log(line: String): Unit = ()

  def trace: String => Request[IO] => TraceBuilder[IO] = { name => request =>
    // In a real environment, this would be where you could establish a new
    // tracing context and inject that fresh header value.
    log(s"Expecting all requests to have ${traceHeaderKey} header.")
    traceBuilder(request.headers.get(traceHeaderKey.ci).get.value)
  }

  def traceBuilder(parentValue: String): TraceBuilder[IO] = { name => httpClient =>
    Client { req =>
      httpClient.run(req.putHeaders(Header(traceHeaderKey, parentValue)))
    }
  }

  test("full tracer: passing headers through multiple levels using authed routes") {
    // Establish the "Address" server
    case class User(name: String)
    val server3: AuthedRoutes[User, IO] =
      new AddressesResource(trace).authedRoutes(new AddressesHandler[IO, User] {
        def getSecureAddress(respond: GetSecureAddressResponse.type)(
            id: String,
            user: User
        )(traceBuilder: TraceBuilder[IO]): IO[GetSecureAddressResponse] =
          IO.pure {
            if (id == "addressId") {
              respond.Ok(
                sdefs.definitions
                  .SecureAddress(Some(s"${user.name}"), Some("line2"), Some("line3"))
              )
            } else respond.NotFound
          }

        def getSecureAddresses(
            respond: GetSecureAddressesResponse.type
        )(user: User)(traceBuilder: TraceBuilder[IO]): IO[GetSecureAddressesResponse] =
          IO.pure(GetSecureAddressesResponse.NotFound)
      })
    val johnDoeBasicAuth = Authorization(BasicCredentials("john-doe", "secret"))
    val authUser: Kleisli[OptionT[IO, ?], Request[IO], User] = Kleisli(
      req =>
        OptionT.fromOption {
          req.headers.collectFirst {
            case auth: Authorization if auth == johnDoeBasicAuth =>
              User("John Doe")
          }
      }
    )
    val middleware          = AuthMiddleware(authUser)
    val httpApp             = middleware(server3).orNotFound
    val secureAddressClient = AddressesClient.httpClient(Client.fromHttpApp(httpApp))
    val testTrace           = traceBuilder("top-level-request")

    val johnDoeResponse: addresses.GetSecureAddressResponse =
      secureAddressClient.getSecureAddress(testTrace, "addressId", headers = List(johnDoeBasicAuth)).unsafeRunSync()

    johnDoeResponse shouldBe addresses.GetSecureAddressResponse
      .Ok(cdefs.definitions.SecureAddress(Some("John Doe"), Some("line2"), Some("line3")))

    val anonymousResponse =
      secureAddressClient
        .getSecureAddress(testTrace, "addressId", headers = List.empty)
        .attempt
        .unsafeRunSync()
        .left
        .value

    anonymousResponse shouldBe UnexpectedStatus(Status.Unauthorized)
  }
}
