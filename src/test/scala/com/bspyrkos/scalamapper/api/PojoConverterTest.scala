package com.bspyrkos.scalamapper.api

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.List

/**
 * Created by spyrkob on 19/07/2014.
 */

class PojoConverterTest extends FlatSpec with Matchers with PojoConverter {

  "Converter" should "copy simple value" in {
    class Foo {def getValue:String = "a"}
    class Bar(var s:String) { def setValue(s:String) {this.s = s} }

    val f = new Foo
    val b = new Bar(null)

    f.getValue _ into (b.setValue)

    b.s should equal("a")
  }

  it should "do nothing if field is empty" in {
    class Foo {def getFooFoo:FooFoo = null}
    class FooFoo { def getValue:String = "a" }
    class Bar(var s:String) {
      def setValue(s:String) {this.s = s}
    }

    val f = new Foo
    val b = new Bar(null)

    f.getFooFoo.getValue _ into (b.setValue)

    b.s shouldBe (null)
  }

  it should "convert string field to integer" in {
    class Foo {def getValue:String = "1"}
    class Bar(var i:Int) { def setValue(i:Int) {this.i = i} }

    val f = new Foo
    val b = new Bar(0)

    (f.getValue _).as(_.toInt) into b.setValue

    b.i shouldBe (1)
  }

  it should "convert list of fields to integer" in {
    class Foo {def getValues = "1"::"2"::Nil}
    class Bar(var s:Seq[Int]) {def setValues(s:Seq[Int]) {this.s=s}}
    val f = new Foo
    val b = new Bar(null)

    (f.getValues _).as(_.toInt) into (b.setValues)

    b.s should be (List(1,2))
  }


}
