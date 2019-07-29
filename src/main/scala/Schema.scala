import com.eclipsesource.json.{Json, JsonValue}

sealed trait Schema[A]{
  def write[Dst](a: A, writer: Writer[Dst]): Dst
}

object Schema {

  // Schema definition
  object IntSchema extends Schema[Int] {
    override def write[Dst](a: Int, writer: Writer[Dst]): Dst = writer.write(a)
  }

  // ...
  case class SeqSchema[B](subSchema: Schema[B]) extends Schema[Seq[B]] {
    override def write[Dst](a: Seq[B], writer: Writer[Dst]): Dst = writer.write(a, subSchema)
  }

  // ...
  case class MappedSchema[A, B](aToB: A => B, subSchema: Schema[B]) extends Schema[A] {
    override def write[Dst](a: A, writer: Writer[Dst]): Dst = subSchema.write(aToB(a), writer)
  }

}

sealed trait Writer[Dst]{
  def write(a: Int): Dst
  def write[A](s: Seq[A], subSchema: Schema[A]): Dst

  // combine base conversion using schema
  def write[A](a: A, schema: Schema[A]): Dst = schema.write(a, this)
}

// base implementations of conversion to "Dst"
trait Dst // some output type, used below

object JsonWriter extends Writer[JsonValue]{
  override def write(a: Int): JsonValue = Json.value(a)
  override def write[A](a: Seq[A], subSchema: Schema[A]): JsonValue = {
    val array = Json.array()
    a.foreach(item => subSchema.write(item, this))
    array
  }
}

