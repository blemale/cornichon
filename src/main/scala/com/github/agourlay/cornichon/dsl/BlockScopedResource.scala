package com.github.agourlay.cornichon.dsl

import com.github.agourlay.cornichon.core.{ CornichonError, Session }

import scala.concurrent.Future

trait BlockScopedResource {

  val sessionTarget: String

  val openingTitle: String
  val closingTitle: String

  def startResource(): Future[ResourceHandle]
}

trait ResourceHandle extends CloseableResource {
  def resourceResults(): Future[Either[CornichonError, Session]]
  val initialisedSession: Either[CornichonError, Session]
}

trait CloseableResource {
  def stopResource(): Future[Unit]
}