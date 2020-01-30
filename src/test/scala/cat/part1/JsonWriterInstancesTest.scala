package cat.part1

import org.scalatest._

class JsonWriterInstancesTest extends FlatSpec with Matchers {
  import JsonWriterInstances._

  val person = Person("razertory", "razertory@163.com")

  val expect = JsObject(
    Map(
      "name" -> JsString(person.name),
      "email" -> JsString(person.email)
    )
  )

  "ToJson" should("return JsonObject") in {
    Json.toJson(person).shouldBe(expect)
    person.toJson.shouldBe(expect)
  }
}
