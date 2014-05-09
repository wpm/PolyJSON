package wpm

object PolyJSON {

  trait Annotation

  case class Token(start: Int, end: Int) extends Annotation

  case class PartOfSpeech(pos: String) extends Annotation

  def main(args: Array[String]) {
    import AnnotationJSON._

    val token = Token(3, 5)
    println(token.prettyPrint)
    require(jsonToAnnotation(token) == token)

    val pos = PartOfSpeech("noun")
    println(pos.prettyPrint)
    require(jsonToAnnotation(pos) == pos)
  }
}
