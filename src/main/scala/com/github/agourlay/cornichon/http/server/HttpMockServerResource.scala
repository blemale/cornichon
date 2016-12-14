package com.github.agourlay.cornichon.http.server

import com.github.agourlay.cornichon.CornichonFeature
import com.github.agourlay.cornichon.core.Session
import com.github.agourlay.cornichon.dsl.{ BlockScopedResource, ResourceHandle }
import com.github.agourlay.cornichon.http.server.HttpMockServerResource.SessionKeys._
import io.circe.Json

case class HttpMockServerResource(interface: Option[String], label: String, portRange: Option[Range]) extends BlockScopedResource {
  val sessionTarget: String = label
  val openingTitle: String = s"Starting HTTP mock server '$label'"
  val closingTitle: String = s"Shutting down HTTP mock server '$label'"

  implicit val (_, ec, system, mat, _) = CornichonFeature.globalRuntime

  def startResource() = {
    CornichonFeature.reserveGlobalRuntime()
    val mockRequestHandler = MockServerRequestHandler(label)
    val akkaServer = new AkkaHttpServer(interface, portRange, mockRequestHandler.requestHandler)
    akkaServer.startServer().map { serverCloseHandler ⇒
      new ResourceHandle {
        def resourceResults() = requestsResults(mockRequestHandler)

        val initialisedSession = Session.newEmpty.addValue(s"$label-url", serverCloseHandler._1)

        def stopResource() = serverCloseHandler._2.stopResource().map { _ ⇒
          mockRequestHandler.shutdown()
          CornichonFeature.releaseGlobalRuntime()
        }
      }
    }
  }

  def requestsResults(mockRequestHandler: MockServerRequestHandler) =
    mockRequestHandler.fetchRecordedRequestsAsJson().map { jsonRequests ⇒
      Session.newEmpty
        .addValues(Seq(
          s"$sessionTarget$receivedBodiesSuffix" → Json.fromValues(jsonRequests).spaces2,
          s"$sessionTarget$nbReceivedCallsSuffix" → jsonRequests.size.toString
        ))
    }
}

object HttpMockServerResource {
  object SessionKeys {
    val nbReceivedCallsSuffix = "-nb-received-calls"
    val receivedBodiesSuffix = "-received-bodies"
  }
}
