package net.vanfleteren.objectvalidation

import org.scalatest.{Matchers, FunSuite}
import org.scalactic.Accumulation._
import org.scalactic._
import DefaultValidations._

import scala.language.implicitConversions

class ValidatieTest extends FunSuite with Matchers with OrAssertions  {

  test("validate passing") {
    val address = AddressDraft("kortrijk", Some("straat"))
    val person = PersonDraft("Jef", Some(address))

    val result = validate(
      "name" -> ok(person.name).when(notEmpty),
      "address" -> valid(person.address)
    )

    result shouldBe Pass
  }


  test("validate failing") {
    val address = AddressDraft("kortrijk", Some("s"))
    val person = PersonDraft("Jef", Some(address))

    val result = validate(
      "name" -> ok(person.name).when(notEmpty),
      "address" -> valid(person.address)
    )

    result shouldBe Fail(One(Error("Lengte moet minstens 2 zijn","address.street")))
  }

  test("validate option if present") {
    val address = AddressDraft("kortrijk", Some("s"))
    val person = PersonDraft("Jef", Some(address))

    val result = validated(PersonDraft.tupled)(
      "name" -> ok(person.name).when(notEmpty),
      "address" -> valid(person.address)
    )

    result.bad { bad =>
      bad.size shouldBe 1
      bad.exists(_.path == "address.street")
    }
  }

  test("validate option if None") {
    val person = PersonDraft("Jef", None)

    val result = validated(PersonDraft.tupled)(
      "name" -> ok(person.name).when(notEmpty),
      "address" -> valid(person.address)
    )

    result.shouldBeGood
  }

  test("validate nested fails when nested fails") {

    val address = Address("kortrijk", "s")
    val person = Person("  ", address)

    val result = validated(Person.tupled)(
      "name" -> ok(person.name).when(notEmpty),
      "address" -> valid(person.address)
    )

    result.bad { bad =>
      bad.size shouldBe 2
      bad.exists(_.path == "name")
      bad.exists(_.path == "address.street")
    }

  }

  test("stay in same type ok") {

    val draft = AddressDraft("kortrijk", None)

    val result = validated(AddressDraft.tupled)(
      "city" -> ok(draft.city).when(notEmpty),
      "street" -> optional(draft.street).when(opt(minLength(2)))
    )

    result.good { good =>
      good.street shouldBe None
    }
  }

  test("stay in same type nok") {

    val draft = AddressDraft("kortrijk", Some("a"))

    val result = validated(AddressDraft.tupled)(
      "address.city" -> ok(draft.city).when(notEmpty),
      "address.street" -> optional(draft.street).when(opt(minLength(2)))
    )

    result.bad { bad =>
      bad.size shouldBe 1
    }

  }

  test("start from draft ok") {

    val draft = AddressDraft("kortrijk", Some("straat"))

    val result = validated(Address.tupled)(
      "address.city" -> ok(draft.city).when(notEmpty),
      "address.street" -> require(draft.street).when(minLength(2))
    )

    result.good { good =>
      good.street shouldBe "straat"
    }

  }

  test("start from draft nok") {

    val draft = AddressDraft("kortrijk", None)

    val result = validated(Address.tupled)(
      "address.city" -> ok(draft.city).when(notEmpty),
      "address.street" -> require(draft.street).when(minLength(2))
    )

    result.bad { bad =>
      bad.size shouldBe 1
    }
  }

  case class Person(name: String, address: Address)

  case class PersonDraft(name: String, address: Option[AddressDraft])

  case class Address(city: String, street: String)

  case class AddressDraft(city: String, street: Option[String])

  implicit def validateAddress(address: AddressDraft): AddressDraft Or Every[Error] = validated(AddressDraft.tupled)(
    "city" -> ok(address.city).when(notEmpty),
    "street" -> optional(address.street).when(opt(minLength(2)))
  )

  implicit def validateAddress(address: Address): Address Or Every[Error] = validated(Address.tupled)(
    "city" -> ok(address.city).when(notEmpty),
    "street" -> ok(address.street).when(minLength(2))
  )


}
