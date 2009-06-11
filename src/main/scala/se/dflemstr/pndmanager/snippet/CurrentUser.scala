package se.dflemstr.pndmanager.snippet

import model._
import _root_.net.liftweb.http._
import S._
import _root_.net.liftweb.util._
import Helpers._
import _root_.net.liftweb.mapper._
import _root_.scala.xml._
import _root_.java.util.Locale

class CurrentUser {
  def online(html: NodeSeq): NodeSeq = if(User.loggedIn_?) html else NodeSeq.Empty

  def offline(html: NodeSeq): NodeSeq = if(!User.loggedIn_?) html else NodeSeq.Empty

  def name: NodeSeq = Text(User.currentUser
         .map(_.niceName) openOr S.?("anonymous"))

  def checkIfNameUnchanged(html: NodeSeq): NodeSeq = {
    val name = User.currentUser.map(_.nickname.is) openOr ""

    if(name.startsWith("change"))
      S.warning(html)
      
    Nil
  }

  def numberOfOwnedPackages: NodeSeq = if(User.loggedIn_?) {
    Text(User.currentUser.open_!.ownedPackages.length.toString)
  } else Text("0")

}
