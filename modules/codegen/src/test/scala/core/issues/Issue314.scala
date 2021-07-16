package core.issues

import com.github.javaparser.ast.Node
import com.twilio.guardrail.generators.Java.Dropwizard
import com.twilio.guardrail.generators.Scala.AkkaHttp
import com.twilio.guardrail.generators.syntax.Scala.companionForStaticDefns
import com.twilio.guardrail.{ Client, Clients, Context, Server, Servers }
import org.scalactic.source
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import support.SwaggerSpecRunner

import scala.language.existentials

class Issue314 extends AnyFunSpec with Matchers with SwaggerSpecRunner {

  private def swagger(config: String) =
    s"""
       |swagger: '2.0'
       |host: localhost:1234
       |paths:
       |  /user/{id}:
       |    get:
       |      operationId: getUser
       |      $config
       |      consumes:
       |        - application/json
       |      produces:
       |        - application/json
       |      parameters:
       |      - name: id
       |        in: path
       |        type: string
       |      responses:
       |        200:
       |          description: Successful
       |""".stripMargin

  describe("JVM, Scala") {

    val flavours = List("jvm", "scala")

    it("should be possible to define an arbitrary class name") {
      flavours.foreach { flavour =>
        verify(s"""
             |      x-$flavour-package: users
             |      x-$flavour-class-prefix: hello
             """.stripMargin, "Hello")
      }
    }

    it("should use the last component of a package name as a class name") {
      flavours.foreach { flavour =>
        verify(s"""
             |      x-$flavour-package: users
             """.stripMargin, "Users")
      }
    }

    it("should fallback to tags in order to determine class name") {
      verify(s"""
           |      tags: [hello]
         """.stripMargin, "Hello")
    }

    it("should be possible to override a class name") {
      flavours.foreach { flavour =>
        verify(
          s"""
             |      tags: [whatever]
             |      x-$flavour-package: users
             |      x-$flavour-class-prefix: hello
             """.stripMargin,
          "Hello"
        )
      }
    }

    def client(prefix: String) =
      s"""class ${prefix}Client(host: String = "http://localhost:1234")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer) {
         |  val basePath: String = ""
         |  private[this] def makeRequest[T: ToEntityMarshaller](method: HttpMethod, uri: Uri, headers: scala.collection.immutable.Seq[HttpHeader], entity: T, protocol: HttpProtocol): EitherT[Future, Either[Throwable, HttpResponse], HttpRequest] = {
         |    EitherT(Marshal(entity).to[RequestEntity].map[Either[Either[Throwable, HttpResponse], HttpRequest]] {
         |      entity => Right(HttpRequest(method = method, uri = uri, headers = headers, entity = entity, protocol = protocol))
         |    }.recover({
         |      case t =>
         |        Left(Left(t))
         |    }))
         |  }
         |  def getUser(id: String, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], GetUserResponse] = {
         |    val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
         |    makeRequest(HttpMethods.GET, host + basePath + "/user/" + Formatter.addPath(id), allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
         |      case StatusCodes.OK =>
         |        resp.discardEntityBytes().future.map(_ => Right(GetUserResponse.OK))
         |      case _ =>
         |        FastFuture.successful(Left(Right(resp)))
         |    }).recover({
         |      case e: Throwable =>
         |        Left(Left(e))
         |    })))
         |  }
         |}""".stripMargin

    def companion(prefix: String) =
      s"""object ${prefix}Client {
         |  def apply(host: String = "http://localhost:1234")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer): ${prefix}Client = new ${prefix}Client(host = host)(httpClient = httpClient, ec = ec, mat = mat)
         |  def httpClient(httpClient: HttpRequest => Future[HttpResponse], host: String = "http://localhost:1234")(implicit ec: ExecutionContext, mat: Materializer): ${prefix}Client = new ${prefix}Client(host = host)(httpClient = httpClient, ec = ec, mat = mat)
         |}""".stripMargin

    def handler(prefix: String) =
      s"trait ${prefix}Handler { def getUser(respond: ${prefix}Resource.GetUserResponse.type)(id: String): scala.concurrent.Future[${prefix}Resource.GetUserResponse] }"

    def resource(prefix: String) =
      s"""object ${prefix}Resource {
         |  def routes(handler: ${prefix}Handler)(implicit mat: akka.stream.Materializer): Route = {
         |    {
         |      path("user" / Segment).apply(id => get(discardEntity(complete(handler.getUser(GetUserResponse)(id)))))
         |    }
         |  }
         |  sealed abstract class GetUserResponse(val statusCode: StatusCode)
         |  case object GetUserResponseOK extends GetUserResponse(StatusCodes.OK)
         |  object GetUserResponse {
         |    implicit def getUserResponseTRM: ToResponseMarshaller[GetUserResponse] = Marshaller { implicit ec =>
         |      resp => getUserResponseTR(resp)
         |    }
         |    implicit def getUserResponseTR(value: GetUserResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
         |      case r: GetUserResponseOK.type =>
         |        scala.concurrent.Future.successful(Marshalling.Opaque {
         |          () => HttpResponse(r.statusCode)
         |        } :: Nil)
         |    }
         |    def apply[T](value: T)(implicit ev: T => GetUserResponse): GetUserResponse = ev(value)
         |    def OK: GetUserResponse = GetUserResponseOK
         |  }
         |}""".stripMargin

    def verify(config: String, expectedClassPrefix: String)(implicit pos: source.Position): Unit = {
      verifyClient(config, expectedClassPrefix)
      verifyServer(config, expectedClassPrefix)
    }

    def verifyClient(config: String, expectedClassPrefix: String)(implicit pos: source.Position): Unit = {
      val (
        _,
        Clients(Client(_, _, _, staticDefns, cls, _) :: _, Nil),
        _
      )       = runSwaggerSpec(swagger(config))(Context.empty, AkkaHttp)
      val cmp = companionForStaticDefns(staticDefns)

      verifyTree(cls.head.value, client(expectedClassPrefix))
      verifyTree(cmp, companion(expectedClassPrefix))
    }

    def verifyServer(config: String, expectedClassPrefix: String)(implicit pos: source.Position): Unit = {
      val (
        _,
        _,
        Servers(Server(_, _, genHandler, genResource :: Nil) :: Nil, Nil)
      ) = runSwaggerSpec(swagger(config))(Context.empty, AkkaHttp)

      verifyTree(genHandler, handler(expectedClassPrefix))
      verifyTree(genResource, resource(expectedClassPrefix))
    }

    def verifyTree(tree: scala.meta.Tree, expectedSyntax: String)(implicit pos: source.Position): Unit =
      normalized(tree.syntax) shouldBe normalized(expectedSyntax)

  }

  describe("Java") {

    it("should be possible to define an arbitrary class name") {
      verify(s"""
           |      x-java-package: users
           |      x-java-class-prefix: hello
             """.stripMargin, "Hello")
    }

    it("should use the last component of a package name as a class name") {
      verify(s"""
           |      x-java-package: users
           """.stripMargin, "Users")
    }

    it("should fallback to tags in order to determine class name") {
      verify(s"""
           |      tags: [hello]
         """.stripMargin, "Hello")
    }

    it("should be possible to override a class name") {
      verify(
        s"""
           |      tags: [whatever]
           |      x-java-package: users
           |      x-java-class-prefix: hello
             """.stripMargin,
        "Hello"
      )
    }

    def verify(config: String, expectedClassPrefix: String)(implicit pos: source.Position): Unit = {
      verifyClient(config, expectedClassPrefix)
      verifyServer(config, expectedClassPrefix)
    }

    def verifyClient(config: String, expectedClassPrefix: String)(implicit pos: source.Position): Unit = {

      val (_, Clients(clt :: _, _), _) =
        runSwaggerSpec(swagger(config))(Context.empty, Dropwizard)
      val cls = clt.client.head.getOrElse(fail("Client does not contain a ClassDefinition"))

      verifyNode(cls, client(expectedClassPrefix))
    }

    def verifyServer(config: String, expectedClassPrefix: String)(implicit pos: source.Position): Unit = {
      val (
        _,
        _,
        Servers(Server(_, _, genHandler, genResource :: Nil) :: Nil, _)
      ) = runSwaggerSpec(swagger(config))(Context.empty, Dropwizard)

      verifyNode(genHandler, handler(expectedClassPrefix))
      verifyNode(genResource, resource(expectedClassPrefix))
    }

    def verifyNode(node: Node, expectedSyntax: String)(implicit pos: source.Position): Unit =
      normalized(node.toString) shouldBe normalized(expectedSyntax)

    def client(prefix: String) =
      s"""public class ${prefix}Client {
         |
         |    public static class Builder {
         |
         |        private static final URI DEFAULT_BASE_URL = URI.create("http://localhost:1234");
         |
         |        private URI baseUrl = DEFAULT_BASE_URL;
         |
         |        private java.util.Optional<Function<Request, java.util.concurrent.CompletionStage<Response>>> httpClient = java.util.Optional.empty();
         |
         |        private java.util.Optional<ObjectMapper> objectMapper = java.util.Optional.empty();
         |
         |        public Builder() {
         |        }
         |
         |        public Builder withBaseUrl(final URI baseUrl) {
         |            this.baseUrl = requireNonNull(baseUrl, "baseUrl is required");
         |            return this;
         |        }
         |
         |        public Builder withHttpClient(final Function<Request, java.util.concurrent.CompletionStage<Response>> httpClient) {
         |            this.httpClient = java.util.Optional.ofNullable(httpClient);
         |            return this;
         |        }
         |
         |        public Builder withObjectMapper(final ObjectMapper objectMapper) {
         |            this.objectMapper = java.util.Optional.ofNullable(JacksonSupport.configureObjectMapper(objectMapper));
         |            return this;
         |        }
         |
         |        private Function<Request, java.util.concurrent.CompletionStage<Response>> getHttpClient() {
         |            return this.httpClient.orElseGet(() -> AsyncHttpClientSupport.createHttpClient(AsyncHttpClientSupport.createDefaultAsyncHttpClient()));
         |        }
         |
         |        private ObjectMapper getObjectMapper() {
         |            return this.objectMapper.orElseGet(() -> JacksonSupport.configureObjectMapper(new ObjectMapper()));
         |        }
         |
         |        public ${prefix}Client build() {
         |            return new ${prefix}Client(this);
         |        }
         |    }
         |
         |    public static class GetUserCallBuilder {
         |
         |        private final RequestBuilder builder;
         |
         |        private final Function<Request, java.util.concurrent.CompletionStage<Response>> httpClient;
         |
         |        private final ObjectMapper objectMapper;
         |
         |        private GetUserCallBuilder(final RequestBuilder builder, final Function<Request, java.util.concurrent.CompletionStage<Response>> httpClient, final ObjectMapper objectMapper) {
         |            this.builder = builder;
         |            this.httpClient = httpClient;
         |            this.objectMapper = objectMapper;
         |        }
         |
         |        public GetUserCallBuilder withHeader(final String name, final String value) {
         |            this.builder.addHeader(name, value);
         |            return this;
         |        }
         |
         |        public java.util.concurrent.CompletionStage<GetUserResponse> call() throws ClientException {
         |            return this.httpClient.apply(builder.build()).thenApply(response -> {
         |                switch(response.getStatusCode()) {
         |                    case 200:
         |                        return new GetUserResponse.Ok();
         |                    default:
         |                        throw new HttpError(response);
         |                }
         |            });
         |        }
         |    }
         |
         |    private final URI baseUrl;
         |
         |    private final Function<Request, java.util.concurrent.CompletionStage<Response>> httpClient;
         |
         |    private final ObjectMapper objectMapper;
         |
         |    private ${prefix}Client(final Builder builder) {
         |        this.baseUrl = builder.baseUrl;
         |        this.httpClient = builder.getHttpClient();
         |        this.objectMapper = builder.getObjectMapper();
         |    }
         |
         |    public GetUserCallBuilder getUser(final String id) {
         |        final RequestBuilder builder = new RequestBuilder("GET").setUrl(this.baseUrl.toString() + "/user/" + Shower.getInstance().show(id));
         |        return new GetUserCallBuilder(builder, this.httpClient, this.objectMapper);
         |    }
         |}""".stripMargin

    def handler(prefix: String) =
      s"""public interface ${prefix}Handler {
         |
         |    @javax.annotation.Generated(value = "com.twilio.guardrail.generators.Java.DropwizardServerGenerator$$")
         |    abstract class GetUserResponse {
         |
         |        private final int statusCode;
         |
         |        GetUserResponse(final int statusCode) {
         |            this.statusCode = statusCode;
         |        }
         |
         |        public int getStatusCode() {
         |            return this.statusCode;
         |        }
         |
         |        @javax.annotation.Generated(value = "com.twilio.guardrail.generators.Java.DropwizardServerGenerator$$")
         |        public static class Ok extends GetUserResponse {
         |
         |            private Ok() {
         |                super(200);
         |            }
         |        }
         |
         |        public static final Ok Ok = new Ok();
         |    }
         |
         |    java.util.concurrent.CompletionStage<${prefix}Handler.GetUserResponse> getUser(final String id);
         |}""".stripMargin

    def resource(prefix: String) =
      s"""@Path("/user/{id}")
         |public class ${prefix}Resource {
         |
         |    private static final Logger logger = LoggerFactory.getLogger(${prefix}Resource.class);
         |
         |    private final ${prefix}Handler handler;
         |
         |    private final java.util.Optional<RequestTimeout> requestTimeout;
         |
         |    public ${prefix}Resource(final ${prefix}Handler handler, final RequestTimeout requestTimeout) {
         |        this.handler = handler;
         |        this.requestTimeout = java.util.Optional.ofNullable(requestTimeout);
         |    }
         |
         |    @Inject
         |    public ${prefix}Resource(final ${prefix}Handler handler) {
         |        this.handler = handler;
         |        this.requestTimeout = java.util.Optional.empty();
         |    }
         |
         |    @GET
         |    public void getUser(@NotNull @PathParam("id") final String id, @Suspended final AsyncResponse asyncResponse) {
         |        getGetUserRequestTimeout().ifPresent(_requestTimeout -> {
         |            asyncResponse.setTimeout(_requestTimeout.getTimeout().toNanos(), java.util.concurrent.TimeUnit.NANOSECONDS);
         |            asyncResponse.setTimeoutHandler(t -> {
         |                if (!asyncResponse.isDone()) {
         |                    asyncResponse.resume(_requestTimeout.getTimeoutResponse());
         |                }
         |            });
         |        });
         |        this.handler.getUser(id).whenComplete((result, err) -> {
         |            if (err != null) {
         |                logger.error("${prefix}Handler.getUser threw an exception ({}): {}", err.getClass().getName(), err.getMessage(), err);
         |                asyncResponse.resume(Response.status(500).build());
         |            } else {
         |                final Response.ResponseBuilder builder = Response.status(result.getStatusCode());
         |                asyncResponse.resume(builder.build());
         |            }
         |        });
         |    }
         |
         |    protected java.util.Optional<RequestTimeout> getGetUserRequestTimeout() {
         |        return this.requestTimeout;
         |    }
         |}""".stripMargin

  }

  private def normalized(s: String): String = s.replaceAll("(?s)\\s+", " ").trim
}
