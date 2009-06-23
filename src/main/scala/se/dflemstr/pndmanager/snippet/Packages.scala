package se.dflemstr.pndmanager.snippet

import se.dflemstr.pndmanager.model.Package
import _root_.scala.xml.{NodeSeq,Text}

/** Provides various package management snippets */
class Packages {
  /** A snippet that inserts the package count where it's used */
  def count: NodeSeq = Text(Package.count.toString)

  //TODO: Add more statistics modules
}
