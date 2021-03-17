package org.codeoncoffee.boxer

import cats.data.NonEmptyList
import cats.data.Validated._
import cats.implicits._

/**
 * Cats Validations
 */
trait BoardValidations {

  def checkEmpty(input: RawInput): ErrorsOrInput = {
    if (input.nonEmpty)
      input.validNel
    else
      BoardError("Input cannot be empty", EmptyInputError).invalidNel
  }

  def checkSameLength(input: RawInput): ErrorsOrInput = {
    if (input.map(_.length).distinct.length == 1)
      input.validNel
    else
      BoardError("All input rows must be the same length", SameLengthError).invalidNel
  }

  def checkMinLength(input: RawInput): ErrorsOrInput = {

    val width = input.head.length
    if (width > 1 || input.length > 1)
      input.validNel
    else
      BoardError("Input rows must contain at least 2 characters in either dimension", MinimumLengthError).invalidNel
  }

  def checkValidChars(input: RawInput): ErrorsOrInput = {
    val distinctChars = input.foldLeft("")(_ + _).toSeq.distinct.sortWith(_ > _)

    if (distinctChars.forall(Seq('-', '*').contains(_)))
      input.validNel
    else
      BoardError("Only '-' and '*' are allowed in the input", InvalidCharError).invalidNel
  }

  def runChecks(input: RawInput): Either[NonEmptyList[BoardError], RawInput] = {
    (checkEmpty(input)
      productR checkSameLength(input)
      productR checkMinLength(input)
      productR checkValidChars(input)).toEither
  }

}
