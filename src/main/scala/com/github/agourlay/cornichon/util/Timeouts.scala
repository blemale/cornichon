package com.github.agourlay.cornichon.util

import java.util.{ Timer, TimerTask }

import com.github.agourlay.cornichon.core.CornichonError

import scala.concurrent.{ ExecutionContext, Future, Promise }
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

object Timeouts {

  // Credits goes to https://github.com/johanandren/futiles/blob/master/src/main/scala/markatta/futiles/Timeouts.scala#L32
  // Copied here because needed only a single method from the jar and added Future version
  def timeout[A](waitFor: FiniteDuration)(what: ⇒ A)(implicit ec: ExecutionContext, timer: Timer): Future[A] = {
    val promise = Promise[A]()
    timer.schedule(new TimerTask {
      override def run(): Unit = {
        // make sure we do not block the timer thread
        Future {
          promise.complete(Try { what })
        }
      }
    }, waitFor.toMillis)

    promise.future
  }

  def timeoutF[A](waitFor: FiniteDuration)(what: ⇒ Future[A])(implicit ec: ExecutionContext, timer: Timer): Future[A] = {
    val promise = Promise[A]()
    timer.schedule(new TimerTask {
      override def run(): Unit = {
        // make sure we do not block the timer thread
        Future {
          promise.completeWith(what)
        }
      }
    }, waitFor.toMillis)

    promise.future
  }

  def errorAfter[A, B](after: FiniteDuration)(what: ⇒ Future[A])(error: CornichonError)(implicit ec: ExecutionContext, timer: Timer): Future[Either[CornichonError, A]] = {
    val timeoutValue = timeoutF(after)(Future.successful(Left(error)))
    Future.firstCompletedOf(Seq(timeoutValue, what.map(Right(_))))
  }

}