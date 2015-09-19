package com.github.agourlay.cornichon.core

import com.github.agourlay.cornichon.CornichonFeature
import org.scalatest.{ Matchers, WordSpec }

trait ScalaTestIntegration extends WordSpec {
  this: CornichonFeature ⇒

  feature.name must {
    "pass all scenarios" in {
      val featureExecution = runFeature()
      featureExecution match {
        case s: SuccessFeatureReport ⇒
          assert(true)
        case FailedFeatureReport(_, _, errors) ⇒
          fail(errors.mkString(" "))
      }
    }
  }
}
