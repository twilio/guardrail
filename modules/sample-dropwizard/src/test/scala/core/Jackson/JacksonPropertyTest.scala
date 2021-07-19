package core.Jackson

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.exc.ValueInstantiationException
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import raw.client.dropwizard.definitions.Foo

import scala.util.Try

class JacksonPropertyTest extends AnyFreeSpec with Matchers {
  private final val mapper = new ObjectMapper

  private val badStr = "{}"
  private val goodStr = """{ "id": 5 }"""

  "Missing required primitive property should not deserialize" in {
    Try(mapper.readValue(badStr, classOf[Foo])).failed.get.getClass mustBe classOf[ValueInstantiationException]
  }

  "Present required primitive property should deserialize" in {
    mapper.readValue(goodStr, classOf[Foo]).getId mustBe 5
  }
}
