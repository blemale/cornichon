package com.github.agourlay.cornichon.core

import java.io.{ PrintWriter, StringWriter }

import cats.data.NonEmptyList

trait CornichonError {
  def baseErrorMessage: String
  val causedBy: Option[NonEmptyList[CornichonError]] = None

  lazy val renderedMessage: String = causedBy.fold(baseErrorMessage) { causes ⇒
    s"""$baseErrorMessage
       |caused by:
       |${causes.toList.map(c ⇒ c.renderedMessage).mkString("\nand\n")}
     """.stripMargin
  }
}

object CornichonError {
  def genStacktrace(exception: Throwable) = {
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    exception.printStackTrace(pw)
    sw.toString
  }

  def fromThrowable(exception: Throwable): CornichonError = exception match {
    case ce: CornichonError ⇒ ce
    case _                  ⇒ StepExecutionError(exception)
  }

  def toException(ce: CornichonError) =
    new RuntimeException(ce.renderedMessage)
}

case class StepExecutionError[A](e: Throwable) extends CornichonError {
  val baseErrorMessage = s"exception thrown '${CornichonError.genStacktrace(e)}'"
}

case class BasicError(error: String) extends CornichonError {
  val baseErrorMessage = error
}