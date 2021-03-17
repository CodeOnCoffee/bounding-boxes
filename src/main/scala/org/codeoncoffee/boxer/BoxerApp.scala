package org.codeoncoffee.boxer

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.sys.{error, exit}
import wvlet.airframe.launcher.{Launcher, command, option}

import cats.implicits._

class BoxerApp(@option(prefix = "-h,--help", description = "show help", isHelp = true)
               displayHelp: Boolean) {

  @command(isDefault = true)
  def default(): Unit = {

    val input = readIn(Seq.empty[String])

    // Read StdIn and create a board
    Board.fromInputValidated(input) match {
      case Left(errors) =>
        // print errors and exit
        Console.err.print(errors.mkString_("\n"))
        doExit(-1)
      case Right(board) =>
        // find all bounding boxes around clusters of '*'
        val allBoxes = board.allBoxes

        // Remove overlapping boxes
        val nonOverlappingBoxes = allBoxes.filterNot(box => box.overlapsAny(allBoxes.filterNot(_ == box)))

        // Group by size. Take largest group
        val largestBoxes = nonOverlappingBoxes.groupBy(_.size).values.toSeq.lastOption.getOrElse(Nil)

        // print Boxes in (y,x)(y,x),... format
        print(largestBoxes.show)

    }
  }

  /**
   * Read StdIn one line at a time.
   *
   * @param value payload of lines
   * @return Sequence of input as Strings
   */
  @tailrec
  private def readIn(value: RawInput): RawInput = {
    readLine match {
      case line if line == null => value    // End of input
      case line => readIn(value :+ line)    // Add row and continue reading
    }
  }

  // for tests to override and prevent JVM exit
  private[boxer] def doExit(code: Int): Unit = {
    exit(-1)
  }
}

object BoxerApp extends App {
  Launcher.execute[BoxerApp](args)
}

