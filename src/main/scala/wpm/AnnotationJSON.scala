package wpm

import spray.json._
import PolyJSON._
import scala.{Boolean, Int}

object AnnotationJSON extends DefaultJsonProtocol {
  def typedFormat[T <: Annotation](untypedFormat: RootJsonFormat[T]) = new RootJsonFormat[T] {
    override def write(obj: T): JsValue = {
      val className = obj.getClass.getSimpleName
      val fv = untypedFormat.write(obj).asJsObject.fields + ("type" -> JsString(className))
      JsObject(fv)
    }

    override def read(json: JsValue) = untypedFormat.read(json)
  }

  implicit object AnnotationFormat extends RootJsonFormat[Annotation] {
    override def write(annotation: Annotation) = {
      def convert(o: Any): JsValue = o match {
        case Some(x) => convert(x)
        case None => JsNull
        case x: String => x.toJson
        case x: Int => x.toJson
        case x: Boolean => x.toJson
        case _ => throw new Exception(s"No JSON conversion for $o")
      }

      val fvs: Seq[(String, Any)] = Seq(("type", annotation.getClass.getSimpleName)) ++
        annotation.getClass.getDeclaredFields.map(field => {
          field setAccessible true
          (field.getName, field.get(annotation))
        })
      val jfvs = for ((f, v) <- fvs.toSeq) yield f -> convert(v)
      JsObject(jfvs: _*)
    }

    override def read(json: JsValue) = {
      val fields = json.asJsObject.fields
      val t = fields("type").convertTo[String]
      t match {
        case "Token" => json.convertTo[Token]
        case "PartOfSpeech" => json.convertTo[PartOfSpeech]
        case _ => throw new Exception(s"Invalid annotation $json")
      }
    }
  }

  implicit val tokenFormat = typedFormat(jsonFormat2(Token))
  implicit val partOfSpeechFormat = typedFormat(jsonFormat1(PartOfSpeech))
}
