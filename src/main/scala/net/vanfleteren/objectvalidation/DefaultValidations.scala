package net.vanfleteren.objectvalidation

import org.scalactic.{Bad, Every, Fail, Good, One, Or, Pass}

/**
 * Methods returning T Or Every[Error] should be used first on the actual value being validated.
 * The methods returning ValidationError can be chained on them later using when.
 *
 * For example:
 *
 *  val x = Some("string")
 *  validate(required(x).when(notEmpty))
 *
 *  will return String Or Every[Error] only when the passed option is defined and has a non empty string.
 *
 */
trait DefaultValidations {

  type ValidationError = org.scalactic.Validation[Error]

  /**
   * The value t should be valid according the the passed validator
   * @param value alue to validate
   * @param validator the actual Validator that will do the validation
   * @tparam T the type of the value
   */
  def valid[T](value: T)(implicit validator: T => T Or Every[Error]): T Or Every[Error] = {
    validator(value)
  }

  /**
   * If the option is defined, it should be valid. If it is None, it is also considered valid.
   */
  def valid[T](t: Option[T])(implicit validator: T => T Or Every[Error]): Option[T] Or Every[Error] = {
    if (t.isDefined) {
      validator(t.get).map(Some(_))
    } else {
      Good(None)
    }
  }

  /**
   * Always passes validation
   */
  def ok[T](t: T): T Or One[Error] = {
    Good(t)
  }

  /**
   * The passed option must be defined.
   */
  def require[T](value: Option[T]): T Or One[Error] = {
    value.map(Good(_)).getOrElse(Bad(One(Error(s"verplicht"))))
  }

  /**
   * The passed option is always good. Acts the same as ok, but might be clearer to use.
   */
  def optional[T](value: Option[T]): Option[T] Or One[Error] = {
    Good(value)
  }


  /**
   * Only executes the passed validation if the option is defined.
   * For example:
   *
   * Since minLength expects an actual String and not an Option, you need to wrap minLength in opt
   *
   * validate(optional(Some("string)).when(opt(minLength(2)))
   *
   *
   * @param validation
   * @param value
   * @tparam T
   * @return
   */
  def opt[T](validation: T => ValidationError)(value: Option[T]): ValidationError = {
    if (value.isDefined) {
      validation(value.get)
    } else {
      Pass
    }
  }


  /**
   * String should be minimum length
   * @param length
   * @param value
   * @return
   */
  def minLength(length: Int)(value: String): ValidationError = {
    if (value.length >= length) {
      Pass
    } else {
      Fail(Error(s"Lengte moet minstens $length zijn"))
    }
  }

  /**
   * String should not be empty
   * @param value
   * @return
   */
  def notEmpty(value: String): ValidationError = {
    if (value != null && value.trim.length > 0) {
      Pass
    } else {
      Fail(Error(s"Mag niet leeg zijn"))
    }
  }

  /**
   * The value to be validated should be one of the passed values
   */
  def isOneOf[T](values:Set[T])(value:T): ValidationError = {
    if(values.contains(value)) {
      Pass
    } else {
      Fail(Error(s"$value is not a valid value"))
    }
  }

}

object DefaultValidations extends DefaultValidations
