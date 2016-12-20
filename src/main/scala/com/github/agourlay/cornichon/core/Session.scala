package com.github.agourlay.cornichon.core

import cats.Show
import cats.syntax.show._
import cats.syntax.either._
import cats.syntax.traverse._
import com.github.agourlay.cornichon.json.{ JsonPath, NotStringFieldError }
import com.github.agourlay.cornichon.json.CornichonJson._
import com.github.agourlay.cornichon.util.Instances._
import io.circe.Json

import scala.collection.immutable.HashMap

case class Session(private val content: Map[String, Vector[String]]) {

  def getOpt(key: String, stackingIndice: Option[Int] = None): Option[String] =
    for {
      values ← content.get(key)
      value ← stackingIndice.fold(values.lastOption) { indice ⇒ values.lift(indice) }
    } yield value

  def get(key: String, stackingIndice: Option[Int] = None): Either[CornichonError, String] =
    Either.fromOption(getOpt(key, stackingIndice), KeyNotFoundInSession(key, stackingIndice, this))

  def getJson(key: String, stackingIndice: Option[Int] = None, path: String = JsonPath.root): Either[CornichonError, Json] =
    for {
      sessionValue ← get(key, stackingIndice)
      jsonValue ← parseJson(sessionValue)
      extracted ← Either.catchNonFatal(JsonPath.run(path, jsonValue)).leftMap(CornichonError.fromThrowable)
    } yield extracted

  def getJsonStringField(key: String, stackingIndice: Option[Int] = None, path: String = JsonPath.root) =
    for {
      json ← getJson(key, stackingIndice, path)
      field ← Either.fromOption(json.asString, NotStringFieldError(json, path))
    } yield field

  def getJsonOpt(key: String, stackingIndice: Option[Int] = None): Option[Json] = getOpt(key, stackingIndice).flatMap(s ⇒ parseJson(s).toOption)

  def getList(keys: Seq[String]): Either[CornichonError, List[String]] = keys.toList.traverseU(v ⇒ get(v))

  def getHistory(key: String): Vector[String] = content.getOrElse(key, Vector.empty)

  def addValue(key: String, value: String): Either[CornichonError, Session] =
    if (key.trim.isEmpty) Left(EmptyKeyException(this))
    else {
      val newSession = content.get(key).fold(Session(content + (key → Vector(value)))) { values ⇒
        Session((content - key) + (key → values.:+(value)))
      }
      Right(newSession)
    }

  def addValues(tuples: Seq[(String, String)]) = tuples.foldLeft(Either.right[CornichonError, Session](this))((s, t) ⇒ s.flatMap(_.addValue(t._1, t._2)))

  def removeKey(key: String) = Session(content - key)

  def merge(otherSession: Session) =
    copy(content = content ++ otherSession.content)

  val prettyPrint =
    content.toSeq
      .sortBy(_._1)
      .map(pair ⇒ pair._1 + " -> " + pair._2.toIterator.map(_.show).mkString("Values(", ", ", ")"))
      .mkString("\n")
}

object Session {
  def newEmpty = Session(HashMap.empty)
  implicit val showSession = new Show[Session] {
    def show(s: Session) = s.prettyPrint
  }
}

case class SessionKey(name: String, index: Option[Int] = None) {
  def atIndex(index: Int) = copy(index = Some(index))
}

case class EmptyKeyException(s: Session) extends CornichonError {
  val baseErrorMessage = s"key value can not be empty - session is \n${s.prettyPrint}"
}

case class KeyNotFoundInSession(key: String, indice: Option[Int], s: Session) extends CornichonError {
  val baseErrorMessage = s"key '$key'${indice.fold("")(i ⇒ s" at indice '$i'")} can not be found in session \n${s.prettyPrint}"
}