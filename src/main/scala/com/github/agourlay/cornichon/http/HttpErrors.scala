package com.github.agourlay.cornichon.http

import cats.Show
import cats.syntax.show._
import com.github.agourlay.cornichon.core.CornichonError

import scala.concurrent.duration.FiniteDuration

sealed trait HttpError extends CornichonError

case class TimeoutErrorAfter[A: Show](request: A, after: FiniteDuration) extends HttpError {
  val baseErrorMessage =
    s"""|${request.show}
        |connection timed out error after ${after.toMillis} ms""".trim.stripMargin
}

case class RequestError[A: Show](request: A, e: Throwable) extends HttpError {
  val baseErrorMessage =
    s"""|${request.show}
        |encountered the following error:
        |${CornichonError.genStacktrace(e)}""".trim.stripMargin
}

case class UnmarshallingResponseError(e: Throwable, response: String) extends HttpError {
  val baseErrorMessage = s"""HTTP response '$response' generated error:
               |${CornichonError.genStacktrace(e)}""".trim.stripMargin
}

case class SseError(e: Throwable) extends HttpError {
  val baseErrorMessage =
    s"""expected SSE connection but got error:
       |${CornichonError.genStacktrace(e)}""".trim.stripMargin
}

case class WsUpgradeError(status: Int) extends HttpError {
  val baseErrorMessage = s"Websocket upgrade error - status received '$status'"
}

case class StatusNonExpected(expected: Int, response: CornichonHttpResponse) extends HttpError {
  val baseErrorMessage =
    s"""status code expected was '$expected' but '${response.status}' was received with body:
       |${response.body}""".stripMargin
}

case class MalformedHeadersError(error: String) extends CornichonError {
  val baseErrorMessage = s"""parsing headers generated error:
               |$error""".trim.stripMargin
}