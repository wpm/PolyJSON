package wpm

import spray.json._
import PolyJSON._

object AnnotationJSON extends DefaultJsonProtocol {

  implicit object TokenFormat extends RootJsonFormat[Token] {
    val untypedFormat = jsonFormat2(Token)

    override def write(obj: Token) = {
      val fv = untypedFormat.write(obj).asJsObject.fields + ("type" -> JsString("Token"))
      JsObject(fv)
    }

    override def read(json: JsValue) = untypedFormat.read(json)
  }

  implicit object PartOfSpeechFormat extends RootJsonFormat[PartOfSpeech] {
    val untypedFormat = jsonFormat1(PartOfSpeech)

    override def write(obj: PartOfSpeech) = {
      val fv = untypedFormat.write(obj).asJsObject.fields + ("type" -> JsString("PartOfSpeech"))
      JsObject(fv)
    }

    override def read(json: JsValue) = untypedFormat.read(json)
  }

  implicit object AnnotationFormat extends RootJsonFormat[Annotation] {
    override def write(annotation: Annotation) = {
      val fvs: Seq[(String, Any)] = Seq(("type", annotation.getClass.getSimpleName)) ++
        annotation.getClass.getDeclaredFields.map(field => {
          field setAccessible true
          (field.getName, field.get(annotation))
        })
      val jfvs = for ((f, v) <- fvs.toSeq if v != None;
                      jv = v match {
                        // TODO Support Option types that are not strings.
                        case Some(o) => JsString(o.toString)
                        case None => JsNull
                        case i: Int => JsNumber(i)
                        case b: Boolean => JsBoolean(b)
                        case s => JsString(s.toString)
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
