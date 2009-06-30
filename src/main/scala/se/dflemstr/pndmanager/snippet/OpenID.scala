package se.dflemstr.pndmanager.snippet

import _root_.se.dflemstr.pndmanager.model._
import _root_.scala.xml._

/** Provides various OpenID-related snippets */
class OpenID {
  /** Creates a link out of some nodes that links to the OpenID website */
  def link(html: NodeSeq) = <a href="http://openid.net/" class="openid-url">{html}</a>;
}
