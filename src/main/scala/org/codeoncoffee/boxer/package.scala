package org.codeoncoffee

import cats.Show
import cats.data.ValidatedNel
import cats.implicits.toShow

import scala.collection.immutable.HashSet
import scala.collection.mutable

package object boxer {

  type BoundingBoxes = Seq[Box]
  type RawInput = Seq[String]
  type Cluster = Seq[Coord]

  case class Coord(x: Int, y: Int, on: Boolean = true) {
    // Specs call for 1-based output, we stay zero-based and translate it here
    override def toString: String = s"(${y + 1},${x + 1})"
  }

  case class Box(topLeft: Coord, bottomRight: Coord) {
    private val xRange: Range = Range.inclusive(topLeft.x, bottomRight.x)
    private val yRange: Range = Range.inclusive(topLeft.y, bottomRight.y)

    /**
     * checks if this Box overlaps another
     * @param other Box to check against
     * @return true if it overlaps the given Box
     */
    def overlaps(other: Box): Boolean =
      xRange.intersect(other.xRange).nonEmpty && yRange.intersect(other.yRange).nonEmpty

    /**
     * Area of the box. Used to sort boxes
     * @return area
     */
    def size: Int = {
      (bottomRight.x - topLeft.x) * (bottomRight.y - topLeft.y)
    }

    /**
     * Check if this box overlaps any of the others
     * @param otherBoxes Boxes to check against
     * @return true if this box overlaps any of the ones given
     */
    def overlapsAny(otherBoxes: BoundingBoxes): Boolean = {
      otherBoxes match {
        case empty if empty.isEmpty => false // only one box
        case others =>
          others.exists(other => {
            other.overlaps(this)
          })
      }
    }

    override def toString: String = s"$topLeft$bottomRight"
  }

  case class Row(cols: Cluster) {
    def apply(col: Int): Option[Coord] = {
      cols.lift(col)
    }
  }

  sealed trait BoardErrorType

  case object EmptyInputError extends BoardErrorType

  case object SameLengthError extends BoardErrorType

  case object MinimumLengthError extends BoardErrorType

  case object InvalidCharError extends BoardErrorType

  case class BoardError(msg: String, error: BoardErrorType)

  type ErrorsOrInput = ValidatedNel[BoardError, RawInput]

  implicit val s: Show[BoardError] = Show.show(error => error.msg)

  implicit object Ordering extends Ordering[Coord] {
    override def compare(a: Coord, b: Coord): Int = a.y compareTo b.y match {
      case 0 => a.x compareTo b.x
      case nonZero => nonZero
    }
  }

  implicit val showBoundingBoxes: Show[BoundingBoxes] = Show.show(_.map(_.show).mkString(","))
  implicit val showCoord: Show[Coord] = Show.fromToString[Coord]
  implicit val showBox: Show[Box] = Show.fromToString[Box]


}
