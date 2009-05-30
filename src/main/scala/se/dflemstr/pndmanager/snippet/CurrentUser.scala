package se.dflemstr.pndmanager.snippet

import model._
import _root_.net.liftweb.http._
import S._
import _root_.net.liftweb.util._
import Helpers._
import _root_.scala.xml._

class CurrentUser {
  def online(html: NodeSeq): NodeSeq = if(User.loggedIn_?) html else NodeSeq.Empty

  def offline(html: NodeSeq): NodeSeq = if(!User.loggedIn_?) html else NodeSeq.Empty

  def name(html: NodeSeq): NodeSeq = Text(User.currentUser.map(_.shortName) openOr "anonymous")
}
