package com.github.agourlay.cornichon.json

import cats.syntax.show._

import com.github.agourlay.cornichon.CornichonFeature
import com.github.agourlay.cornichon.dsl.Dsl
import com.github.agourlay.cornichon.json.JsonSteps._
import com.github.agourlay.cornichon.json.CornichonJson._

trait JsonDsl {
  this: CornichonFeature with Dsl ⇒

  val root = JsonPath.root

  def show_key_as_json(key: String, indice: Option[Int] = None) = show_session(key, indice, v ⇒ v.flatMap(s ⇒ parseJson(s).map(_.show)))

  def session_json_values(k1: String, k2: String) = JsonValuesStepBuilder(k1, k2, resolver)

}
