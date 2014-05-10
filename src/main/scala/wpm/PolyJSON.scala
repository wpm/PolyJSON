package wpm

import spray.json._

object PolyJSON {

  trait Annotation

  case class Token(start: Int, end: Int) extends Annotation

  case class PartOfSpeech(pos: String) extends Annotation

  def main(args: Array[String]) {
    import AnnotationJSON._

    val token = Token(3, 5)
    println(token.toJson.prettyPrint)
    require(token.toJson.convertTo[Annotation] == token)

    val pos = PartOfSpeech("noun")
    println(pos.toJson.prettyPrint)
    require(pos.toJson.convertTo[Annotation] == pos)

    val tokens:Seq[Annotation] = Seq(Token(3, 5), PartOfSpeech("noun"))
    println(tokens.toJson.prettyPrint)
    require(tokens.toJson.convertTo[Seq[Annotation]] == tokens)
  }
}
