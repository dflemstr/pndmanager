package se.dflemstr.pndmanager.snippet

import _root_.scala.xml._
import _root_.net.liftweb.http._
import _root_.net.liftweb.builtin.snippet._

/** Loads a translated NodeSeq with the specified name */
class Locns extends DispatchSnippet {
  def dispatch: DispatchIt = {
    case _ => render _
  }
  def render(data: NodeSeq) = //a very ugly hack indeed
    XML.loadString("<xml:group>" + Unparsed(Loc.render(data).text) + "</xml:group>")
}
