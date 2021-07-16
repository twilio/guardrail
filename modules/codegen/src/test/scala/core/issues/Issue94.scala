package core.issues

import com.twilio.guardrail.generators.Http4s
import com.twilio.guardrail.{ClassDefinition, Context, ProtocolDefinitions, StaticDefns}
import org.scalatest.{FunSpec, Matchers}
import support.SwaggerSpecRunner

import scala.meta._

class Issue94 extends FunSpec with Matchers with SwaggerSpecRunner {

  describe("ReadOnly parameter should be returned from server") {

    val swagger: String = """
      | openapi: "3.0.0"
      | info:
      |   title: Server should write out readOnly parameters.
      |   version: 1.0.0
      | components:
      |   schemas:
      |     ObjectWithWriteOnly:
      |       type: object
      |       properties:
      |         prop:
      |           type: string
      |           writeOnly: true
      |     ObjectWithReadOnly:
      |       type: object
      |       properties:
      |         prop:
      |           type: string
      |           readOnly: true""".stripMargin

    val (
      ProtocolDefinitions(
      ClassDefinition(_, _, _, StaticDefns(_, _, writeOnlyEncoder :: _ :: Nil), _) ::
      ClassDefinition(_, _, _, StaticDefns(_, _, readOnlyEncoder :: _ :: Nil), _) :: Nil, _, _, packageObjectContents
      ), // ProtocolDefinitions
      _, // clients
      _, // Servers
    ) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    it("Ensure correct code is generated") {

      val expectedReadOnlyResult =
        q"""
           implicit def encodeObjectWithReadOnly(implicit ec: EncodingContext) = {
             val readOnlyKeys = Set[String]("prop")
             val writeOnlyKeys = Set[String]()
             Encoder.forProduct1("prop")((o: ObjectWithReadOnly) => o.prop).mapJsonObject(_.filterKeys(ec.filter(readOnlyKeys, writeOnlyKeys)))
           }
         """

      val expectedWriteOnlyResult =
        q"""
           implicit def encodeObjectWithWriteOnly(implicit ec: EncodingContext) = {
             val readOnlyKeys = Set[String]()
             val writeOnlyKeys = Set[String]("prop")
             Encoder.forProduct1("prop")((o: ObjectWithWriteOnly) => o.prop).mapJsonObject(_.filterKeys(ec.filter(readOnlyKeys, writeOnlyKeys)))
           }
         """

      readOnlyEncoder.structure shouldBe expectedReadOnlyResult.structure
      writeOnlyEncoder.structure shouldBe expectedWriteOnlyResult.structure
    }
  }
}
