package se.dflemstr.pndmanager.snippet

import scala.xml.{NodeSeq, Text}

class Application {

  /** Returns the name of the application */
  def name: NodeSeq = Text("PND Package Manager")

  /** Tidifies some XHTML content */
  def tidy(xhtml: NodeSeq): NodeSeq = xhtml //TODO: apply a "tidy" filter on this (indentation fixes etc)
}
