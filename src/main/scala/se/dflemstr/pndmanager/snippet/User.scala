package se.dflemstr.pndmanager.snippet

import _root_.se.dflemstr.pndmanager.model._
import _root_.net.liftweb._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import _root_.scala.xml._
import _root_.java.util.Locale

/** Provides various snippets that are useful for user management */
class User {
  /** Returns the snippet contents if an user is logged in */
  def online(html: NodeSeq): NodeSeq = if(User.loggedIn_?) html else NodeSeq.Empty

  /** Returns the snippet contents if an user isn't logged in */
  def offline(html: NodeSeq): NodeSeq = if(User.notLoggedIn_?) html else NodeSeq.Empty

  /** Returns the name of the user, if it has one */
  def name: NodeSeq = Text(User.currentUser
         .map(_.niceName) openOr S.?("user.anonymous"))

  /** Emits a warning if the user has a "default" nickname */
  def checkIfNameUnchanged(html: NodeSeq): NodeSeq = {
    val name = User.currentUser.map(_.nickname.is) openOr ""

    if(name.startsWith("change"))
      S.warning(html)
      
    Nil
  }

  /** Returns the numebr of packages that the user owns */
  def numberOfOwnedPackages: NodeSeq = if(User.loggedIn_?) {
    Text(User.currentUser.open_!.ownedPackages.length.toString)
  } else Text("0")

  /** Creates a login form using the snippet contents as the template */
  def loginForm(html: NodeSeq) = User.login(html)

  /** Logs the user out immediately, if included on a page */
  def logoutCommand = {
    User.logout
    S.redirectTo("/")
  }

  def logoutButton = SHtml.ajaxButton(S.?("user.logout"), () => {
    User.logoutCurrentUser
    js.JsCmds.RedirectTo("/")
  })

  /** Creates a profile edit form for the logged in user */
  def editForm(html: NodeSeq) = User.edit(html)
  
}
