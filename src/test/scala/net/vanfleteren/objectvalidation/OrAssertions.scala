package net.vanfleteren.objectvalidation

import org.scalactic.Or
import org.scalatest.Matchers
import org.scalatest.exceptions.TestFailedException


trait OrAssertions extends Matchers {

  implicit class RichOr[G,B](validation:G Or B) {

    def bad(assertFunc: B => Unit) : G Or B = {
      if (validation.isBad) {
        validation.badMap(assertFunc)
        validation
      } else {
        throw new TestFailedException("The Or is not bad: "+validation.get, 0)
      }
    }

    def good(assertFunc: G => Unit) : G Or B = {
      if (validation.isGood) {
        validation.map(assertFunc)
        validation
      } else {
        throw new TestFailedException("The Or is not Good: "+validation.swap.get, 0)
      }
    }

    def shouldBeGood = {
      validation.isGood shouldBe true
    }
  }

}

