package wpm

import spray.json._

/**
 * A JSON protocol that serializes case classes as JSON with the type of the case class written in as a type field in
 * the JSON object.
 *
 * To implement a protocol:
 * - Create an object that extends [[TypedJSONProtocol]]
 * - Make each case class to be serialized import TypedJSONSerializable
 * - Implement the readTypedJson that converts JSON object with type string to Option case classes or returns None if
 * the type is not recognized
 */
trait TypedJSONProtocol extends DefaultJsonProtocol {
  val TYPE: String = "type"

  trait TypedJSONSerializable

  def readTypedJson(t: String, json: JsValue): Option[TypedJSONSerializable]

  /**
   * JSON serializer of an object that writes the class name in as a type field.
   * @param untypedFormat serializer that does not write the type information, e.g. [[jsonFormat1]], [[jsonFormat2]]
   * @tparam T the class to be serialized
   * @return implicit object that supports typed serialization of T by this protocol
   */
  def typedFormat[T <: TypedJSONSerializable](untypedFormat: RootJsonFormat[T]) = new RootJsonFormat[T] {
    override def write(obj: T): JsValue = {
      val className = obj.getClass.getSimpleName
      val fv = untypedFormat.write(obj).asJsObject.fields + (TYPE -> JsString(className))
      JsObject(fv)
    }

    override def read(json: JsValue) = untypedFormat.read(json)
  }

  /**
   * JSON serializer of the base [[TypedJSONSerializable]] type.
   *
   * This is needed in addition to the serializers for the individual subtypes of because of type erasure when these
   * objects appear in collections.
   */
  implicit object TypedJSONSerializableFormat extends RootJsonFormat[TypedJSONSerializable] {
    override def write(obj: TypedJSONSerializable) = {
      def convert(o: Any): JsValue = o match {
        case Some(x) => convert(x)
        case None => JsNull
        case x: String => x.toJson
        case x: Int => x.toJson
        case x: Boolean => x.toJson
        case _ => throw new Exception(s"No JSON conversion for $o")
      }

      val fvs: Seq[(String, Any)] = Seq((TYPE, obj.getClass.getSimpleName)) ++
        obj.getClass.getDeclaredFields.map(field => {
          field setAccessible true
          (field.getName, field.get(obj))
        })
      val jfvs = for ((f, v) <- fvs.toSeq) yield f -> convert(v)
      JsObject(jfvs: _*)
    }

    override def read(json: JsValue) = {
      val fields = json.asJsObject.fields
      val t = fields(TYPE).convertTo[String]
      readTypedJson(t, json) match {
        case Some(obj) => obj
        case None => throw new Exception(s"Invalid typed JSON $json")
      }
    }
  }

}
