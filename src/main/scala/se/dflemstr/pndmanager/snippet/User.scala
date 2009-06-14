package se.dflemstr.pndmanager.snippet

import model._
import _root_.net.liftweb._
import util._
import http._
import sitemap._
import Loc._
import _root_.scala.xml._
import _root_.java.util.Locale

/** Provides various snippets that are useful for user management */
class User {
  def online(html: NodeSeq): NodeSeq = if(User.loggedIn_?) html else NodeSeq.Empty

  def offline(html: NodeSeq): NodeSeq = if(User.notLoggedIn_?) html else NodeSeq.Empty

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

  def loginForm(html: NodeSeq) = User.login(html)

  def logoutCommand = User.logout

  def editForm(html: NodeSeq) = User.edit(html)
  
}
