package se.dflemstr.pndmanager.snippet

import model._
import _root_.net.liftweb.http._
import S._
import _root_.net.liftweb.util._
import Helpers._
import _root_.scala.xml._

class OpenID {
  def link(html: NodeSeq) = <a href="http://openid.net/" class="openid-url">{html}</a>;
}
