package wpm

import spray.json._
import DefaultJsonProtocol._
import PolyJSON._

object AnnotationJSON {
  implicit def jsonToAnnotation(json: JsValue): Annotation = {
    val fields = json.asJsObject.fields
    val t = fields("type").convertTo[String]
    val fv = fields - "type"
    t match {
      case "Token" => Token(fv("start").convertTo[Int], fv("end").convertTo[Int])
      case "PartOfSpeech" => PartOfSpeech(fv("pos").convertTo[String])
      case _ => throw new Exception(s"Invalid annotation $json")
    }
  }

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
}
