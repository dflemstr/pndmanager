package se.dflemstr.pndmanager.snippet

import model._
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.http.js.JE._
import _root_.scala.xml._

/** Provides various package management snippets */
class Packages {
  /** A snippet that inserts the package count where it's used */
  def count: NodeSeq = Text(Package.count.toString)

  def categoryDiagram: NodeSeq = Nil

  //TODO: Add more statistics modules
}
