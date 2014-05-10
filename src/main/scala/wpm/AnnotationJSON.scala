package wpm

import spray.json._
import PolyJSON._

object AnnotationJSON extends DefaultJsonProtocol {
  def typedFormat[T <: Annotation](untypedFormat: RootJsonFormat[T]) = new RootJsonFormat[T] {
    override def write(obj: T): JsValue = {
      val className = obj.getClass.getSimpleName
      val fv = untypedFormat.write(obj).asJsObject.fields + ("type" -> JsString(className))
      JsObject(fv)
    }

    override def read(json: JsValue) = untypedFormat.read(json)
  }

  implicit val tokenFormat = typedFormat(jsonFormat2(Token))
  implicit val partOfSpeechFormat = typedFormat(jsonFormat1(PartOfSpeech))

  implicit object AnnotationFormat extends RootJsonFormat[Annotation] {
    override def write(annotation: Annotation) = {
      def convertPrimitive(p: Any) = p match {
        case None => JsNull
        case i: Int => JsNumber(i)
        case b: Boolean => JsBoolean(b)
        case s: String => JsString(s.toString)
        case _ => throw new Exception(s"Non JSON primitive $p")
      }

      val fvs: Seq[(String, Any)] = Seq(("type", annotation.getClass.getSimpleName)) ++
        annotation.getClass.getDeclaredFields.map(field => {
          field setAccessible true
          (field.getName, field.get(annotation))
        })
      val jfvs = for ((f, v) <- fvs.toSeq if v != None;
                      jv = v match {
                        case Some(o) => convertPrimitive(o)
                        case None => JsNull
                        case _ => convertPrimitive(v)
                      }
      ) yield f -> jv
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

}
