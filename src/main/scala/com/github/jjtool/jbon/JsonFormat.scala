package com.github.jjtool.jbon

import java.io.IOException

import com.eclipsesource.json.{Json, JsonObject, JsonValue}

import scala.util.{Failure, Success, Try}

trait JsonFormat[A]{
  def read(json: JsonValue): Try[A]
  def write(a: A): JsonValue
  def transform[B](r: A => B, w: B => A): JsonFormat[B] = JsonFormat.MappedJsonFormat(this, r, w)
}

object JsonFormat {

  case object booleanFormat extends JsonFormat[Boolean] {
    override def read(json: JsonValue): Try[Boolean] = safe(json.asBoolean())
    override def write(a: Boolean): JsonValue = Json.value(a)
  }

  case object intFormat extends JsonFormat[Int] {
    override def read(json: JsonValue): Try[Int] = safe(json.asInt())
    override def write(a: Int): JsonValue = Json.value(a)
  }

  case object floatFormat extends JsonFormat[Float] {
    override def read(json: JsonValue): Try[Float] = safe(json.asFloat())
    override def write(a: Float): JsonValue = Json.value(a)
  }

  case object doubleFormat extends JsonFormat[Double] {
    override def read(json: JsonValue): Try[Double] = safe(json.asDouble())
    override def write(a: Double): JsonValue = Json.value(a)
  }

  case object stringFormat extends JsonFormat[String] {
    override def read(json: JsonValue): Try[String] = safe(json.asString())
    override def write(a: String): JsonValue = Json.value(a)
  }

  case class ArrayFormat[A](implicit aFormat: JsonFormat[A]) extends JsonFormat[Array[A]] {

    override def read(json: JsonValue): Try[Array[A]] = {
      val jsonArray = json.asArray()
      val itemNumber = jsonArray.size()
      val array = new Array[A](itemNumber)
      var i = 0
      while(i < itemNumber){
        val item = jsonArray.get(i)
        aFormat.read(item) match {
          case Success(a) => array(i) = a
          case Failure(t) => return Failure(ReadFailure(s"item $i", t))
        }
        i += 1
      }
      Success(array)
    }

    override def write(as: Array[A]): JsonValue = {
      val array = Json.array()
      as.foreach(a => array.add(aFormat.write(a)))
      array
    }
  }

  /** A JsonFormat[B] made from JsonFormat[A] function to transform A<=>B */
  case class MappedJsonFormat[A,B](
    af: JsonFormat[A],
    mapRead: A => B,
    mapWrite: B => A,
  ) extends JsonFormat[B] {
    override def read(json: JsonValue): Try[B] = af.read(json).map(mapRead)
    override def write(b: B): JsonValue = af.write(mapWrite(b))
  }

  case class Object1Format[O,F1](
    field1: String, get1: O => F1, format1: JsonFormat[F1]
  )(
    build: (F1) => O
  ) extends JsonFormat[O] {
    override def read(json: JsonValue): Try[O] = for {
      obj <- getObject(json)
      f1 <- format1.read(obj.get(field1)) >> (t => ReadFailure(field1, t))
    } yield build(f1)

    override def write(o: O): JsonValue = {
      val obj = Json.`object`()
      obj.add(field1, format1.write(get1(o)))
    }
  }

  case class Object2Format[O,F1, F2](
    field1: String, get1: O => F1, format1: JsonFormat[F1],
    field2: String, get2: O => F2, format2: JsonFormat[F2]
  )(
    build: (F1, F2) => O
  ) extends JsonFormat[O] {
    override def read(json: JsonValue): Try[O] = for {
      obj <- getObject(json)
      f1 <- format1.read(obj.get(field1)) >> (t => ReadFailure(field1, t))
      f2 <- format2.read(obj.get(field2)) >> (t => ReadFailure(field2, t))
    } yield build(f1, f2)

    override def write(o: O): JsonValue = {
      val obj = Json.`object`()
      obj.add(field1, format1.write(get1(o)))
      obj.add(field2, format2.write(get2(o)))
    }
  }

  // TODO: tests
  // TODO: test failure msg

  private def getObject(json: JsonValue): Try[JsonObject] = Try(json.asObject()) match {
    case s: Success[JsonObject] => s
    case Failure(t) => Failure(ReadFailure("", t))
  }

  private implicit class WrapFailure[A](val fail: Try[A]) extends AnyVal {
    def >>(wrap: Throwable => ReadFailure[A]): Try[A] = fail match {
      case s: Success[A] => s
      case Failure(t) => Failure(wrap(t))
    }
  }

  /** Failure while reading Json */
  case class ReadFailure[A](path: String, cause: Throwable) extends IOException(path, cause)

  private def safe[A](f: => A): Try[A] = Try(f).recoverWith{
    case t => Failure(ReadFailure("", t))
  }

}
