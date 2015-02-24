package net.vanfleteren.objectvalidation

import org.scalactic.{One, Fail, Pass}
import org.scalatest.{Matchers, FunSuite}

import DefaultValidations._
import org.scalactic.Accumulation._

class DefaultValidationsTest extends FunSuite with Matchers with OrAssertions {

  test("oneOf") {

    isOneOf(Set("foo","bar"))("bar") shouldBe Pass

    isOneOf(Set("foo","bar"))("xxx") shouldBe Fail(Error("xxx is not a valid value"))

  }

}
