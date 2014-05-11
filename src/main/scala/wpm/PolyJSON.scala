package wpm

import spray.json._

import AnnotationJSONProtocol._

case class Token(start: Int, end: Int) extends TypedJSONSerializable

case class PartOfSpeech(pos: String) extends TypedJSONSerializable

object AnnotationJSONProtocol extends TypedJSONProtocol {
  implicit val tokenFormat = typedFormat(jsonFormat2(Token))
  implicit val partOfSpeechFormat = typedFormat(jsonFormat1(PartOfSpeech))

  override def readTypedJson(t: String, json: JsValue) = t match {
    case "Token" => Some(json.convertTo[Token])
    case "PartOfSpeech" => Some(json.convertTo[PartOfSpeech])
    case _ => None
  }
}

object PolyJSON {

  def main(args: Array[String]) {

    val token = Token(3, 5)
    println(token.toJson.prettyPrint)
    require(token.toJson.convertTo[TypedJSONSerializable] == token)

    val pos = PartOfSpeech("noun")
    println(pos.toJson.prettyPrint)
    require(pos.toJson.convertTo[TypedJSONSerializable] == pos)

    val tokens: Seq[TypedJSONSerializable] = Seq(Token(3, 5), PartOfSpeech("noun"))
    println(tokens.toJson.prettyPrint)
    require(tokens.toJson.convertTo[Seq[TypedJSONSerializable]] == tokens)
  }
}
