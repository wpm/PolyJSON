package wpm

import spray.json._
import PolyJSON._

object AnnotationJSON extends DefaultJsonProtocol {
  implicit def annotationToJson(annotation: Annotation): JsObject = {
    val fvs: Seq[(String, Any)] = Seq(("type", annotation.getClass.getSimpleName)) ++
      annotation.getClass.getDeclaredFields.map(field => {
        field setAccessible true
        (field.getName, field.get(annotation))
      })
    val jfvs = for ((f, v) <- fvs.toSeq if v != None;
                    jv = v match {
                      case Some(x) => JsString(x.toString)
                      case x: Int => JsNumber(x)
                      case x => JsString(x.toString)
                    }) yield f -> jv
    JsObject(jfvs: _*)
  }

  implicit def jsonToAnnotation(json: JsValue): Annotation = {
    val fields = json.asJsObject.fields
    val t = fields("type").convertTo[String]
    t match {
      case "Token" => json.convertTo[Token]
      case "PartOfSpeech" => json.convertTo[PartOfSpeech]
      case _ => throw new Exception(s"Invalid annotation $json")
    }
  }

  implicit val tokenFormat = jsonFormat2(Token)
  implicit val partOfSpeechFormat = jsonFormat1(PartOfSpeech)
}
