package com.github.agourlay.cornichon.steps.regular

import java.util.Timer

import cats.syntax.either._
import cats.data.NonEmptyList

import scala.concurrent.{ ExecutionContext, Future }
import com.github.agourlay.cornichon.core._
import com.github.agourlay.cornichon.core.Done._
import com.github.agourlay.cornichon.core.Engine._

case class DebugStep(message: Session ⇒ Either[CornichonError, String], title: String = "Debug step") extends Step {

  def setTitle(newTitle: String) = copy(title = newTitle)

  //TODO deduplicate
  override def run(engine: Engine)(initialRunState: RunState)(implicit ec: ExecutionContext, timer: Timer) = {
    val (fullLogs, xor) = Either.catchNonFatal(message(initialRunState.session))
      .fold(
        e ⇒ {
          val errors = NonEmptyList.of(CornichonError.fromThrowable(e))
          val debugErrorLogs = errorLogs(title, errors, initialRunState.depth)
          val failedStep = FailedStep(this, errors)
          (debugErrorLogs, Left(failedStep))
        },
        debugMessageXor ⇒ {
          debugMessageXor.fold(
            e ⇒ {
              val errors = NonEmptyList.of(e)
              val debugErrorLogs = errorLogs(title, errors, initialRunState.depth)
              val failedStep = FailedStep(this, errors)
              (debugErrorLogs, Left(failedStep))
            },
            debugMessage ⇒ {
              val runLogs = Vector(DebugLogInstruction(debugMessage, initialRunState.depth))
              (runLogs, rightDone)
            }
          )
        }
      )
    Future.successful(initialRunState.appendLogs(fullLogs), xor)
  }
}

object DebugStep {
  def fromString(message: String) = DebugStep(s ⇒ Right(message))
  def fromFctString(message: Session ⇒ String) = DebugStep(s ⇒ Right(message(s)))
}