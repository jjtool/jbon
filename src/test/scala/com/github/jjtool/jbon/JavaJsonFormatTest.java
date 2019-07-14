package com.github.jjtool.jbon;

public class JavaJsonFormatTest {

  class ClassA {
    int i;
    String s;
    ClassA(int i, String s){
      this.i = i;
      this.s = s;
    }
  }

  class ClassB {
    double d;
    ClassA[] as;
    ClassB(double d, ClassA[] as){
      this.d = d;
      this.as = as;
    }
  }

  public void test1(){
    JsonFormatTest.writeAndRead(true, JsonFormat.booleanFormat$.MODULE$);
    System.out.println("java test 1"); // TODO: remove
  }

// TODO: that's not working, as expected need a dedicated java api
//  public void objectTest(){
//
//    JsonFormat<ClassA> classAFormat = new JsonFormat.Object2<>(
//      ClassA::new,
//      new JsonFormat.Field<>("i", a -> a.i, ),
//      new JsonFormat.Field<>("s", a -> a.s, JsonFormat.stringFormat$.MODULE$)
//    );
//
//    JsonFormat<ClassB> classBFormat = new JsonFormat.Object2<>(
//      ClassB::new,
//      new JsonFormat.Field<>("d", b -> b.d, ),
//      new JsonFormat.Field<>("d", b -> b.as, new JsonFormat.ArrayFormat<>(classAFormat, ))
//    );
//  }
}
