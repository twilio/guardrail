package swagger

import java.util

import cats.data.NonEmptyList
import com.twilio.guardrail.extract.VendorExtension
import com.twilio.guardrail.generators.Scala.AkkaHttp
import com.twilio.guardrail.{ ClassDefinition, Client, Clients, CodegenTarget, Context, ProtocolDefinitions }
import io.swagger.parser.OpenAPIParser
import io.swagger.v3.parser.core.models.ParseOptions
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import support.SwaggerSpecRunner

import scala.collection.JavaConverters._

class VendorExtensionTest extends AnyFunSuite with Matchers with SwaggerSpecRunner {

  val spec: String = s"""
    |swagger: '2.0'
    |host: petstore.swagger.io
    |produces:
    | - application/json
    |x-scala-garbage: io.swagger.models.Swagger
    |paths:
    |  foo:
    |    x-scala-garbage: io.swagger.models.Path
    |    get:
    |      x-scala-garbage: io.swagger.models.Operation
    |      parameters:
    |      - name: id
    |        in: query
    |        required: true
    |        type: integer
    |        x-scala-garbage: io.swagger.models.parameters.Parameter
    |      responses:
    |        '200':
    |          description: successful operation
    |          x-scala-garbage: io.swagger.models.Response
    |          schema:
    |            "$$ref": "#/definitions/Order"
    |        '404':
    |          description: Order not found
    |          x-scala-garbage: io.swagger.models.Response
    |definitions:
    |  Order:
    |    type: object
    |    x-scala-garbage: io.swagger.models.Model
    |    properties:
    |      id:
    |        type: integer
    |        format: int64
    |        x-scala-garbage: io.swagger.models.properties.Property
    |""".stripMargin

  test("Able to extract strings") {
    val parseOpts = new ParseOptions
    parseOpts.setResolve(true)
    val swagger = new OpenAPIParser().readContents(spec, new util.LinkedList(), parseOpts).getOpenAPI
    // VendorExtension(swagger).extract[String]("x-scala-garbage") should equal(Some("io.swagger.models.Swagger"))
    for {
      (k, v) <- swagger.getPaths.asScala
      _ = VendorExtension(v).extract[String]("x-scala-garbage") should equal(Some("io.swagger.models.Path"))
      op <- v.readOperations().asScala
      _ = VendorExtension(op).extract[String]("x-scala-garbage") should equal(Some("io.swagger.models.Operation"))
      _ = for {
        param <- op.getResponses.asScala.values
        // _ = VendorExtension(param).extract[String]("x-scala-garbage") should equal(Some("io.swagger.models.parameters.Parameter"))
      } ()
      _ = for {
        (_, resp) <- op.getResponses.asScala
        _ = VendorExtension(resp)
          .extract[String]("x-scala-garbage") should equal(Some("io.swagger.models.Response"))
      } ()
    } ()

    for {
      (_, defn) <- swagger.getComponents.getSchemas.asScala
      _ = VendorExtension(defn).extract[String]("x-scala-garbage") should equal(Some("io.swagger.models.Model"))
      _ = for {
        (_, prop) <- defn.getProperties.asScala
        _ = VendorExtension(prop)
          .extract[String]("x-scala-garbage") should equal(Some("io.swagger.models.properties.Property"))
      } ()
    } ()
  }

  private val itemsOverrideSpec =
    s"""
       |openapi: 3.0.1
       |info:
       |  title: Foo
       |  version: 0.0.0
       |components:
       |  schemas:
       |    Foo:
       |      type: object
       |      required:
       |        - arrayprop
       |      properties:
       |        arrayprop:
       |          type: array
       |          items:
       |            type: string
       |            x-jvm-type: SomethingElse
       |paths:
       |  /foo:
       |    get:
       |      operationId: getFoo
       |      parameters:
       |        - name: arrayquery
       |          in: query
       |          required: true
       |          schema:
       |            type: array
       |            items:
       |              type: string
       |              x-jvm-type: SomethingElse
       |        - name: regularquery
       |          in: query
       |          required: true
       |          schema:
       |            type: string
       |            x-jvm-type: SomethingElse
       |      requestBody:
       |        required: true
       |        content:
       |          application/json:
       |            schema:
       |              required:
       |                - arrayprop
       |              properties:
       |                arrayprop:
       |                  type: array
       |                  items:
       |                    type: string
       |                    x-jvm-type: SomethingElse
       |      responses:
       |        200: {}
       |""".stripMargin

  test("CustomTypeName works on array items") {
    val (
      ProtocolDefinitions(ClassDefinition("Foo", _, _, protoCls, _, _) :: Nil, _, _, _, _),
      Clients(Client(_, _, _, _, NonEmptyList(Right(clientCls), Nil), _) :: Nil, _),
      _
    ) =
      runSwaggerSpec(itemsOverrideSpec)(Context.empty, AkkaHttp, targets = NonEmptyList.one(CodegenTarget.Client))

    protoCls.syntax shouldBe """case class Foo(arrayprop: Vector[SomethingElse] = Vector.empty)"""
    clientCls.syntax should include(
      "def getFoo(arrayquery: Iterable[SomethingElse], regularquery: SomethingElse, arrayprop: Iterable[SomethingElse], headers: List[HttpHeader] = Nil)"
    )
  }
}
