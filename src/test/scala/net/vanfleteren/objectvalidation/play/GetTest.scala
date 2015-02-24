package net.vanfleteren.objectvalidation.play

import org.scalatest.{FreeSpec, Matchers}
import play.api.data.mapping.{Path, Failure, GenericRules, Success}
import play.api.data.validation.ValidationError

class GetTest extends FreeSpec with Matchers {

    object R extends GenericRules
    import R._

    case class Address(street : String, city : String, postcode : String)
    case class Person(name : String, age : Int, address : Address)

    val address = Address("Southover Street", "Brighton", "BN2 9UA")
    val person = Person("Joe Grey", 37, address)

    "Experimental" - {
      import shapeless.test._

      "validate a la carte" - {

        (Get[Person] \ 'age).read(min(0))
        .validate(person) shouldBe Success(person)

        (Get[Person] \ 'age).read(max(0))
        .validate(person) shouldBe Failure(List((Path \ "age", List(ValidationError("error.max", 0)))))

        (Get[Person] \ 'address \ 'city).read(notEmpty)
        .validate(person) shouldBe Success(person)

        (Get[Person] \ 'address \ 'city).read(maxLength(1))
        .validate(person) shouldBe Failure(List((Path \ "address" \ "city", List(ValidationError("error.maxLength", 1)))))

        Get[Person] { __ =>
          (__ \ 'age).read(min(0)) ~>
            (__ \ 'address \ 'city).read(notEmpty)
        }.validate(person) shouldBe Success(person)

        illTyped("""(Get[Person] \ 'address \ 'plip).read(notEmpty)""")  // does not compile

        Get[Person] { __ =>
          (__ \ 'age).read(min(0)) ~>
            (__ \ 'address \ 'city).read(notEmpty)
        }.validate(Person("Joe Grey", -12, address.copy(city = ""))) shouldBe Failure(List((Path \ "age",List(ValidationError("error.min", 0))), (Path \ "address" \ "city", List(ValidationError("error.required")))))
      }
    }


  "Using Person example" - {

    case class Person(name: String, address: Address)

    case class PersonDraft(name: String, address: Option[AddressDraft])

    case class Address(city: String, street: String)

    case class AddressDraft(city: String, street: Option[String])


    val address = AddressDraft(city="city",street = Some("street"))

    //TODO handle option
  //  (Get[AddressDraft] \ 'street).read(notEmpty)
  //  .validate(address) shouldBe Success(address)

  }
}
