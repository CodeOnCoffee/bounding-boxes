package org.codeoncoffee.boxer

import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers._

import java.io.{ByteArrayOutputStream, StringReader}

class BoxerAppTest extends AnyFlatSpec with should.Matchers {
  behavior of "BoxerApp Results"

  it should "parse input correctly" in {
    val (out, err) = runBoxerApp(
      """|----
         |-**-
         |-**-
         |----
         |"""
    )
    out should equal("(2,2)(3,3)")
  }


  it should "ignore diagonal groups" in {
    val (out, err) = runBoxerApp(
      """|*---
         |-*--
         |----
         |----
         |"""
    )
    out should equal("")
  }

  it should "ignore overlapping groups" in {
    val (out, err) = runBoxerApp(
      """|**-------***
         |-*--**--***-
         |-----***--**
         |-------***--
         |"""
    )
    out should equal("(1,1)(2,2)")
  }

  it should "return multiple groups if same size" in {
    val (out, err) = runBoxerApp(
      """|------------
         |----**------
         |-----***----
         |------------
         |---****-----
         |---**-------
         |------------
         |"""
    )
    out should include("(2,5)(3,8)")
    out should include("(5,4)(6,7)")
  }

  it should "support single row matches" in {
    val (out, err) = runBoxerApp(
      """|***
         |"""
    )
    out should include("(1,3)")
  }

  behavior of "BoxerApp Validation"

  it should "enforce same length" in {
    val (out, err) = runBoxerApp(
      """|-----
         |**
         |"""
    )
    err should include("All input rows must be the same length")
  }

  it should "enforce same length, detecting empty lines" in {
    val (out, err) = runBoxerApp(
      """|
        |***
         |"""
    )
    err should include("All input rows must be the same length")
  }


  it should "enforce char limitations for '-' and '*'" in {
    val (out, err) = runBoxerApp(
      """|-----
         |**&--
         |"""
    )
    err should include("Only '-' and '*' are allowed in the input")
  }

  def runBoxerApp(input: String): (String, String) = {

    val ps = new StringReader(input.stripMargin)
    val outCapture = new ByteArrayOutputStream
    val errCapture = new ByteArrayOutputStream
    Console.withIn(ps) {
      Console.withOut(outCapture) {
        Console.withErr(errCapture) {
          new BoxerApp(false) {
            // Prevent JVM exits
            override def doExit(code: Int): Unit = ()
          }.default()
        }
      }
    }

    def getOutput(cap: ByteArrayOutputStream) = new String(cap.toByteArray, "UTF-8")

    (getOutput(outCapture), getOutput(errCapture))

  }
}