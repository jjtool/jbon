package com.github.jjtool.jbon

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.TryValues.convertTryToSuccessOrFailure

class JsonFormatTest extends FlatSpec with Matchers {

  import JsonFormatTest.writeAndRead
  import JsonFormat._

  it should "convert basic type to json and back" in {
    writeAndRead(true, booleanFormat)
    writeAndRead(42, intFormat)
    writeAndRead(3.1415f, floatFormat)
    writeAndRead(math.Pi, doubleFormat)
    writeAndRead("hello", stringFormat)
    writeAndRead(Array(1,2,3), ArrayFormat(intFormat))
  }

  private case class ClassA(i: Int, s: String)
  private case class ClassB(d: Double, as: Seq[ClassA])

  private val a1 = ClassA(1, "1")
  private val a2 = ClassA(2, "2")
  private val b = ClassB(123.456, Array(a1, a2))

  it should "convert case classes to json and back" in {

    val formatA = Object2Format[ClassA, Int, String](
      "i", _.i, intFormat,
      "s", _.s, stringFormat
    )(ClassA)

    val formatB = Object2Format[ClassB, Double, Seq[ClassA]](
      "d", _.d, doubleFormat,
      "as", _.as, SeqFormat(formatA)
    )(ClassB)

    writeAndRead(b, formatB)
  }

  it should "convert case classes to json and back - option 2" in {
    val a1 = ClassA(1, "1")
    val a2 = ClassA(2, "2")
    val b = ClassB(123.456, Array(a1, a2))

    val formatA = Object2(ClassA)(
      Field("i", _.i, intFormat),
      Field("s", _.s, stringFormat)
    )

    val formatB = Object2(ClassB)(
      Field("d", _.d, doubleFormat),
      Field("as", _.as, SeqFormat(formatA))
    )

    writeAndRead(b, formatB)
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
}