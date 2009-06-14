package se.dflemstr.pndmanager.snippet

import model._
import _root_.scala.xml._

/** Provides various package management snippets */
class Packages {
  /** A snippet that inserts the package count where it's used */
  def count: NodeSeq = Text(Package.count.toString)
}
