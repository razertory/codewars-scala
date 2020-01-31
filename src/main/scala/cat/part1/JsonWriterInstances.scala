package cat.part1

sealed trait Json

final case class JsObject(get: Map[String, Json]) extends Json

final case class JsString(get: String) extends Json

final case class JsNumber(get: Double) extends Json

case object JsNull extends Json

final case class Person(name: String, email: String)

trait JsonWriter[A] {
  def write(value: A): Json
}

object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] = {
    (value: String) => JsString(value)
  }

  implicit val personWriter: JsonWriter[Person] = {
    (value: Person) => {
      JsObject(
        Map(
          "name" -> JsString(value.name),
          "email" -> JsString(value.email)
        )
      )
    }
  }

  object Json {
    def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = {
      w.write(value)
    }
  }

  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json = {
      w.write(value)
    }
  }
}

