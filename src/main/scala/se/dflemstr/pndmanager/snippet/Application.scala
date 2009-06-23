package se.dflemstr.pndmanager.snippet

import scala.xml.{NodeSeq, Text, Elem}
import net.liftweb.http.S
import _root_.net.liftweb.sitemap.SiteMap

class Application {

  /** Returns the name of the application */
  def name: NodeSeq = Text("PND Package Manager")

  def goBack(contents: NodeSeq): NodeSeq =
    if(S.referer.isDefined)
      <a href={S.referer.open_!}>{contents}</a>
    else
      NodeSeq.Empty
}
