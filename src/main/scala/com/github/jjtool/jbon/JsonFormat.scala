package com.github.jjtool.jbon

import java.io.IOException

import com.eclipsesource.json.{Json, JsonObject, JsonValue}

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

trait JsonFormat[A]{
  def read(json: JsonValue): Try[A]
  def write(a: A): JsonValue
  def transform[B](r: A => B, w: B => A): JsonFormat[B] = JsonFormat.MappedJsonFormat(this, r, w)
}

object JsonFormat {

  def write[A](a: A, schema: Schema[A]): JsonValue = schema match {
    case Schema.char    => Json.value(a.asInstanceOf[Char])
    case Schema.boolean => Json.value(a.asInstanceOf[Boolean])
    case Schema.int     => Json.value(a.asInstanceOf[Int])
    case Schema.float   => Json.value(a.asInstanceOf[Float])
    case Schema.double  => Json.value(a.asInstanceOf[Double])
    case Schema.string  => Json.value(a.asInstanceOf[String])

    case s: Schema.Array[b] =>
      val subSchema = s.subSchema
      a.asInstanceOf[Array[b]].foldLeft(Json.array()){
        case (array, item) => array.add(write(item, subSchema))
      }

    case s: Schema.Seq[b,_] =>
      val subSchema = s.subSchema
      a.asInstanceOf[Seq[b]].foldLeft(Json.array()){
        case (array, item) => array.add(write(item, subSchema))
      }

    case s: Schema.Object[A] =>
      s.fields.foldLeft(Json.`object`()){
        case (obj, f: Schema.Field[A,b]) => obj.add(f.name, write(f.get(a), f.schema))
      }

    case s: Schema.Mapped[A,_] =>
      write(s.toB(a), s.base)
  }

  def read[A](json: JsonValue, schema: Schema[A]): Try[A] = schema match {
    case Schema.char    => safe(json.asInt().toChar.asInstanceOf[A])
    case Schema.boolean => safe(json.asBoolean().asInstanceOf[A])
    case Schema.int     => safe(json.asInt().asInstanceOf[A])
    case Schema.float   => safe(json.asFloat().asInstanceOf[A])
    case Schema.double  => safe(json.asDouble().asInstanceOf[A])
    case Schema.string  => safe(json.asString().asInstanceOf[A])

    case s: Schema.Array[b] =>
      val jsonArray = json.asArray()
      val itemNumber = jsonArray.size()
      implicit def classTag: ClassTag[b] = s.classTag
      val seq = new Array[b](jsonArray.size())
      var i = 0
      while(i < itemNumber){
        val item = jsonArray.get(i)
        read(item, s.subSchema) match {
          case Success(x) => seq(i) = x
          case Failure(t) => return Failure(ReadFailure(s"item $i", t))
        }
        i += 1
      }
      Success(seq.asInstanceOf[A])

    case s: Schema.Seq[b,c] =>
      val jsonArray = json.asArray()
      val itemNumber = jsonArray.size()
      val builder = s.factory.newBuilder
      var i = 0
      while(i < itemNumber){
        val item = jsonArray.get(i)
        read(item, s.subSchema) match {
          case Success(x) => builder.addOne(x)
          case Failure(t) => return Failure(ReadFailure(s"item $i", t))
        }
        i += 1
      }
      Success(builder.result().asInstanceOf[A])

    case s: Schema.Object1[A, ff1] => for {
      obj <- getObject(json)
      f1 <- readField(obj, s.f1)
    } yield s.build(f1)

    case s: Schema.Object2[A, ff1, ff2] => for {
      obj <- getObject(json)
      f1 <- readField(obj, s.f1)
      f2 <- readField(obj, s.f2)
    } yield s.build(f1, f2)

    case s: Schema.Object3[A, ff1, ff2, ff3] => for {
      obj <- getObject(json)
      f1 <- readField(obj, s.f1)
      f2 <- readField(obj, s.f2)
      f3 <- readField(obj, s.f3)
    } yield s.build(f1, f2, f3)

    case s: Schema.Mapped[A,b] =>
      read(json, s.base).map(s.toA)
  }

  private def readField[F](obj: JsonObject, f: Schema.Field[_,F]): Try[F] = {
    read(obj.get(f.name), f.schema) >> (t => ReadFailure(f.name, t))
  }

  case object booleanFormat extends JsonFormat[Boolean] {
    override def read(json: JsonValue): Try[Boolean] = safe(json.asBoolean())
    override def write(a: Boolean): JsonValue = Json.value(a)
  }

  case object intFormat extends JsonFormat[Int] {
    override def read(json: JsonValue): Try[Int] = safe(json.asInt())
    override def write(a: Int): JsonValue = Json.value(a)
  }

  case object charFormat extends JsonFormat[Char] {
    override def read(json: JsonValue): Try[Char] = safe(json.asInt().toChar)
    override def write(a: Char): JsonValue = Json.value(a)
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

  case class SeqFormat[A](aFormat: JsonFormat[A]) extends JsonFormat[Seq[A]] {

    override def read(json: JsonValue): Try[Seq[A]] = {
      val jsonArray = json.asArray()
      val itemNumber = jsonArray.size()
      var seq = Vector[A]()
      var i = 0
      while(i < itemNumber){
        val item = jsonArray.get(i)
        aFormat.read(item) match {
          case Success(a) => seq :+= a
          case Failure(t) => return Failure(ReadFailure(s"item $i", t))
        }
        i += 1
      }
      Success(seq)
    }

    override def write(as: Seq[A]): JsonValue = {
      val array = Json.array()
      as.foreach(a => array.add(aFormat.write(a)))
      array
    }
  }

  case class ArrayFormat[A](aFormat: JsonFormat[A])(implicit tag: ClassTag[A]) extends JsonFormat[Array[A]] {

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

  case class Field[O,F](name: String, get: O => F, format: JsonFormat[F])

  case class Object1[O, F1](build: (F1) => O)(field1: Field[O,F1]) extends JsonFormat[O] {
    override def read(json: JsonValue): Try[O] = for {
      obj <- getObject(json)
      f1 <- field1.format.read(obj.get(field1.name)) >> (t => ReadFailure(field1.name, t))
    } yield build(f1)

    override def write(o: O): JsonValue = {
      val obj = Json.`object`()
      obj.add(field1.name, field1.format.write(field1.get(o)))
    }
  }

  case class Object2[O, F1, F2](build: (F1, F2) => O)(field1: Field[O,F1], field2: Field[O,F2]) extends JsonFormat[O] {
    override def read(json: JsonValue): Try[O] = for {
      obj <- getObject(json)
      f1 <- field1.format.read(obj.get(field1.name)) >> (t => ReadFailure(field1.name, t))
      f2 <- field2.format.read(obj.get(field2.name)) >> (t => ReadFailure(field2.name, t))
    } yield build(f1, f2)

    override def write(o: O): JsonValue = {
      Json.`object`()
        .add(field1.name, field1.format.write(field1.get(o)))
        .add(field2.name, field2.format.write(field2.get(o)))
    }
  }

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
