package com.github.jjtool.jbon

import scala.collection.{BuildFrom, Factory}
import scala.reflect.ClassTag

sealed trait Schema[A]

object Schema {

  case object char    extends Schema[Char]
  case object boolean extends Schema[Boolean]
  case object int     extends Schema[Int]
  case object float   extends Schema[Float]
  case object double  extends Schema[Double]
  case object string  extends Schema[String]

  case class Array[A](subSchema: Schema[A])(implicit val classTag: ClassTag[A]) extends Schema[scala.Array[A]]
  case class Seq[A,C<:scala.Seq[A]](subSchema: Schema[A])(implicit val factory: Factory[A,C]) extends Schema[C]

  case class Mapped[A,B](base: Schema[B], toB: A => B, toA: B => A) extends Schema[A]


  sealed abstract class Object[A](val fields: Field[A, _]*) extends Schema[A]

  case class Field[Obj, F](name: String, get: Obj => F, schema: Schema[F])

  case class Object1[A, F1](build: F1 => A)(
    val f1: Field[A, F1]
  ) extends Object[A](f1)

  case class Object2[A, F1, F2](build: (F1,F2) => A)(
    val f1: Field[A, F1],
    val f2: Field[A, F2]
  ) extends Object[A](f1,f2)

  case class Object3[A, F1, F2, F3](build: (F1,F2,F3) => A)(
    val f1: Field[A, F1],
    val f2: Field[A, F2],
    val f3: Field[A, F3]
  ) extends Object[A](f1,f2,f3)

  case class Object4[A, F1, F2, F3, F4](build: (F1,F2,F3,F4) => A)(
    val f1: Field[A, F1],
    val f2: Field[A, F2],
    val f3: Field[A, F3],
    val f4: Field[A, F4]
  ) extends Object[A](f1,f2,f3,f4)

  case class Object5[A, F1, F2, F3, F4, F5](build: (F1,F2,F3,F4,F5) => A)(
    val f1: Field[A, F1],
    val f2: Field[A, F2],
    val f3: Field[A, F3],
    val f4: Field[A, F4],
    val f5: Field[A, F5]
  ) extends Object[A](f1,f2,f3,f4,f5)


}
