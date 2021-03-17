package org.codeoncoffee.boxer

import cats.{Monad, Monoid, Show}

import scala.collection.immutable.HashSet
import scala.collection.mutable
import cats.data._
import cats.implicits._

import scala.annotation.tailrec

case class Board(rows: Seq[Cluster]) {

  // access row by position
  def apply(row: Int): Row = {
    Row(rows.lift(row).getOrElse(Nil))
  }

  // Convenience for merging payloads
  implicit val scanPayloadMonoid: Monoid[ExpandPayload] = new Monoid[ExpandPayload] {
    def empty: ExpandPayload = ExpandPayload(Set.empty, Set.empty)
    def combine(x: ExpandPayload, y: ExpandPayload): ExpandPayload =
      ExpandPayload(x.cluster ++ y.cluster, x.scanned ++ y.scanned )
  }

  /**
   * Get a Sequence of all Bounding Boxes around clusters
   */
  def allBoxes: BoundingBoxes = {
    val allStars = rows.flatten.filter(_.on)
    val clusterScanResults = scanForClusters(allStars, ScanPayload(Set.empty, Set.empty))


    val sorted: Set[Cluster] = {
      clusterScanResults.clusters
        .filter(_.size > 1) // ignore single coordinate clusters
        .map(_.toSeq.sorted) // sort coordinates top-left to bottom-right
    }

    // Return bounding boxes
    sorted.toSeq map { coords =>
      Box(
        Coord(coords.map(_.x).min, coords.map(_.y).min),
        Coord(coords.map(_.x).max, coords.map(_.y).max)
      )
    }
  }

  // Recursive payload
  case class ScanPayload(clusters: Set[Set[Coord]], scanned: Set[Coord])

  /**
   * Recursive algorithm to find all clusters of asterisks on the board
   *
   * @param coordsToScan coordinates left to scan
   * @param payload recursive payload
   * @return new payload for next iteration
   */
  @tailrec
  private def scanForClusters(coordsToScan: Cluster, payload: ScanPayload): ScanPayload = {

    coordsToScan match {
      case Nil => payload
      case next :: tail =>
        val expanded = expandCluster(Set(next), ExpandPayload(Set.empty[Coord], payload.scanned))
        val newPayload = ScanPayload(payload.clusters + expanded.cluster, payload.scanned ++ expanded.scanned)
        scanForClusters(tail, newPayload)
    }
  }

  // Recursive payload
  case class ExpandPayload(cluster: Set[Coord], scanned: Set[Coord])

  /**
   * Recursive algorithm to expand out from a single asterisk finding all others connected
   *
   * @param coords coordinates left to scan
   * @param payload recursive payload
   * @return new payload for next iteration
   */
  @tailrec
  private def expandCluster(coords: Set[Coord], payload: ExpandPayload): ExpandPayload = {
    coords.toSeq match {
      case c if c.isEmpty => payload
      case head :: tail =>
        val newPayload = ExpandPayload(payload.cluster + head, payload.scanned + head)
        val surrounding = Set(coordNeighbors(head): _*)
          .filter(_.on)
          .diff(newPayload.cluster)
          .diff(newPayload.scanned)
        expandCluster( surrounding ++ tail, newPayload )
    }
  }

  /**
   * Return a sequence of coordinates in the cardinal directions
   * @param c origin coordinate
   * @return sequence of neighbors
   */
  private def coordNeighbors(c: Coord): Cluster = Seq(
    apply(c.y)(c.x - 1), // Left
    apply(c.y)(c.x + 1), // Right
    apply(c.y - 1)(c.x), // Top
    apply(c.y + 1)(c.x) // Bottom
  ).flatten

}

object Board extends BoardValidations {

  val empty = new Board(Nil)

  /**
   * Parse raw string rows to a validated board or to a list of errors
   *
   * @param input Sequence of Strings representing the rows of a board in their raw form
   * @return Board or a list of errors
   */
  def fromInputValidated(input: RawInput): Either[NonEmptyList[BoardError], Board] = {
    runChecks(input) map { validatedInput =>
      val coords = validatedInput.zipWithIndex.map({ case (row, rowIdx) => row.toSeq.zipWithIndex.map({ case (cell, colIdx) => Coord(colIdx, rowIdx, cell == '*') }) })
      new Board(coords)
    }
  }
}