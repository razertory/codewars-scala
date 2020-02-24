package cat.part1

import org.scalatest._

class JsonWriterInstancesTest extends FlatSpec with Matchers {

  import JsonWriterInstances._

  val person = Person("razertory", "razertory@163.com")
  val personStr = "A string!"

  val expect = JsObject(
    Map(
      "name" -> JsString(person.name),
      "email" -> JsString(person.email)
    )
  )

  "ObjectToJson" should ("return JsonObject") in {
    Json.toJson(person).shouldBe(expect)
    person.toJson.shouldBe(expect)
  }

  "StringToJson" should ("return JsonObject") in {
    println(Json.toJson(personStr))
  }
}
