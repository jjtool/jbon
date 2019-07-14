package com.github.jjtool.jbon

import com.eclipsesource.json.JsonValue

object Hello extends App {
  println("hello")

  jbonize()

}

sealed trait T

case class A(f1: Int, f2: String, f3: Array[Double]) extends T

case object O extends T


