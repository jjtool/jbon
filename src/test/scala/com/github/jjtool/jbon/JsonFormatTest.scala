package com.github.jjtool.jbon

import org.scalatest.TryValues.convertTryToSuccessOrFailure
import org.scalatest.{FlatSpec, Matchers}

class JsonFormatTest extends FlatSpec with Matchers {

  import JsonFormatTest._

  it should "convert basic type to json and back" in {
    writeAndRead2(true, Schema.boolean)
    writeAndRead2(42, Schema.int)
    writeAndRead2(3.1415f, Schema.float)
    writeAndRead2(math.Pi, Schema.double)
    writeAndRead2("hello", Schema.string)
    writeAndRead2(Array(1,2,3), Schema.Array(Schema.int))
  }

  private case class ClassA(i: Int, s: String, c: Char)
  private case class ClassB(d: Double, as: Seq[ClassA])

  private val a1 = ClassA(1, "1", '1')
  private val a2 = ClassA(2, "2", '2')
  private val b = ClassB(123.456, Array(a1, a2))

  it should "convert case classes to json and back" in {
    val schemaA = Schema.Object3(ClassA)(
      Schema.Field("i", _.i, Schema.int),
      Schema.Field("s", _.s, Schema.string),
      Schema.Field("c", _.c, Schema.char)
    )

    val schemaB = Schema.Object2(ClassB)(
      Schema.Field("d", _.d, Schema.double),
      Schema.Field("as", _.as, Schema.Seq(schemaA))
    )

    writeAndRead2(b, schemaB)
  }

  it should "run the java test" in {
    new JavaJsonFormatTest().test1()
  }

  // TODO: test failure msg

}

object JsonFormatTest extends Matchers {
  def writeAndRead[A](item: A, format: JsonFormat[A]): Unit = {
    val json = format.write(item)
    val result = format.read(json)
    result.success.value should equal (item)
  }

  def writeAndRead2[A](item: A, schema: Schema[A]): Unit = {
    val json = JsonFormat.write(item, schema)
    val result = JsonFormat.read(json, schema)
    result.get
    result.success.value should equal (item)
  }
}