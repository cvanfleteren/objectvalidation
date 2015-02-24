package net.vanfleteren

import org.scalactic.{Bad, Every, Fail, Good, One, Or, Pass, Validation => SValidation}
import shapeless.syntax.std.tuple._

package object objectvalidation {

  type Validation[G] = Or[G, Every[Error]]

  /**
   * Run all validations, returning Pass if all were Good, Fail witrh all Errors is there was any Bad.
   */
  def validate(validations: (String, Validation[_])*): SValidation[Every[Error]] = {

    val list: List[(String, Or[_, Every[Error]])] = validations.toList

    val justValidaties = validations.map(_._2)
    if (allGood(justValidaties)) {
      Pass
    } else {
      Fail(extractBad(list).swap.get)
    }
  }


  /**
   * Run all validations. If all validates ok, run the given creator against the Goods of the validations.
   * If any validation didn't pass, return the accumulated Errors.

   * @param creator function to call when all validations returned Good
   * @param validations the actual validations
   * @tparam Domain the type that the creator will create
   * @tparam AG The Good type of the first validation
   * @tparam BG The Good type of the second validation
   * @tparam A the first validation
   * @tparam B the second validation
   */
  def validated[Domain, AG, BG, A <: Validation[AG], B <: Validation[BG]]
  (creator: ((AG, BG)) => Domain)
  (validations: ((String, A), (String, B)))
  : Domain Or Every[Error] = {

    val list: List[(String, Or[_, Every[Error]])] = validations.toList

    val justValidaties = list.map(_._2)
    if (allGood(justValidaties)) {
      Good(creator((validations._1._2.get, validations._2._2.get)))
    } else {
      extractBad(list)
    }
  }

  def validated[Domain, AG, BG, CG, A <: Validation[AG], B <: Validation[BG], C <: Validation[CG]]
  (creator: ((AG, BG, CG)) => Domain)
  (validations: ((String, A), (String, B), (String, C)))
  : Domain Or Every[Error] = {

    val list: List[(String, Or[_, Every[Error]])] = validations.toList

    val justValidaties = list.map(_._2)
    if (allGood(justValidaties)) {
      Good(creator((validations._1._2.get, validations._2._2.get, validations._3._2.get)))
    } else {
      extractBad(list)
    }
  }


  private def allGood(l: Iterable[Or[_, _]]) = l.count(_.isGood) == l.size

  private def extractBad[G](l: List[(String, Or[_, Every[Error]])]): Bad[G, Every[Error]] = {
    val bads = l.filter { case (key, or) => or.isBad}.map { case (key, or) => (key, or.swap.get)}

    val validationErrors = bads.map { case (key, errors) =>
      errors.map { error =>
        if(error.path.isEmpty) {
          error.copy(path = key)
        } else {
          error.copy(path =key+"."+error.path)
        }
      }
    }.flatten
    Bad[G, Every[Error]](Every.from(validationErrors).get)
  }

}
