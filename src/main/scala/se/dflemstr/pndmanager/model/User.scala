package se.dflemstr.pndmanager.model

import scala.xml.Text
import net.liftweb._
import mapper._
import http._
import sitemap.{Loc,Menu}
import Loc._
import SHtml._
import util._
import Helpers._
import net.liftweb.openid._
import scala.xml._

import _root_.org.openid4java.discovery.Identifier
import _root_.org.openid4java.consumer._

object User extends User with MetaOpenIDProtoUser[User] {
  /** The database table in which all users are stored */
  override def dbTableName = "users"

  /** The base URL that is used for user management */
  override val basePath: List[String] = "user" :: Nil
  def openIDVendor = SimpleOpenIdVendor

  //This is unused, since we use external templates
  override def screenWrap = Empty

  /** Create a login form with the specified template */
  def login(xhtml: NodeSeq) = {
    if (S.post_?) {
      S.param("username").
      foreach(username => openIDVendor.loginAndRedirect(username, performLogUserIn))
    }
    def performLogUserIn(openid: Box[Identifier], fo: Box[VerificationResult],
                         exp: Box[Exception]): LiftResponse = {
      (openid, exp) match {
        case (Full(id), _) =>
          val user = this.findOrCreate(id.getIdentifier)
          logUserIn(user)
          S.notice(S.?("welcome.user") replace ("%user%", user.niceName))
        case (_, Full(exp)) =>
          S.error(S.?("report.exception") replace ("%exception%", exp.getMessage))
        case _ =>
          S.error(S.?("report.loginreason") replace ("%reason%", fo.toString))
      }
      RedirectResponse("/")
    }

    bind("user", xhtml,
         "openid" -> (js.JsCmds.FocusOnLoad(<input type="text" name="username"/>)),
         "submit" -> (<input type="submit" value={S.??("log.in")}/>))
  }

  /** Create an edit form with the specified template */
  def edit(xhtml: NodeSeq) = {
    val theUser: User = currentUser.open_! // we know we're logged in

    def testEdit() {
      theUser.validate match {
        case Nil =>
          Log.info(theUser.nickname)
          theUser.save
          S.notice("Profile updated") //TODO: translate!
          S.redirectTo(homePage)

        case xs =>
          S.error(xs)
          S.mapSnippet(S.currentSnippet openOr "", innerEdit)
      }
    }

    def innerEdit(t: NodeSeq) = bind("user", t,
                                "field" -> ((h: NodeSeq) => localForm(h, theUser, true)),
                                "submit" -> SHtml.submit(S.??("edit"), testEdit _))
    innerEdit(xhtml)
  }

  /* Make a form with all the editable fields of an user, from a template */
  protected def localForm(xhtml: NodeSeq, user: User, ignorePassword: Boolean): NodeSeq = {
    signupFields
      .map(fi => getSingleton.getActualBaseField(user, fi)) //get actual fields
      .filter(f => !ignorePassword || (f match { //remove the password field
        case f: MappedPassword[_] => false
        case _ => true
      }))
      .flatMap(f => 
          bind("field", xhtml,
            "name" ->(if(f.displayName == "nickname")
                        Text(S.?("nickname"))
                      else
                        Text(f.displayName)),
            "form" -> f.toForm)
        )
    //(The nickname hack above is there to fix a translation mistake in the Lift Core)
    //TODO: fix this upstream
  }

  /** Find an user by nickname */
  override def findByNickname(str: String): List[User] =
    findAll(By(nickname, str))

  //
  //  The following overrides allow us to create custom templates for User locations
  //

  /**
   * The menu item for login
   */
  override def loginMenuLoc: Box[Menu] =
    Full(Menu(Loc("Login", loginPath, S.??("login"),
                  If(User.notLoggedIn_? _, S.??("already.logged.in")))))

  /**
   * The menu item for logout
   */
  override def logoutMenuLoc: Box[Menu] =
    Full(Menu(Loc("Logout", logoutPath, S.??("logout"), testLogginIn)))

  /**
   * The menu item for editing the user
   */
  override def editUserMenuLoc: Box[Menu] =
    Full(Menu(Loc("EditUser", editPath, S.??("edit.user"), testLogginIn)))

  override def createUserMenuLoc: Box[Menu] =  Empty
  override def lostPasswordMenuLoc: Box[Menu] = Empty
  override def resetPasswordMenuLoc: Box[Menu] = Empty
  override def changePasswordMenuLoc: Box[Menu] = Empty

  //don't want to ruin the staircase effect, leaving space here

  override def validateUserMenuLoc: Box[Menu] = Empty
}

class User extends OpenIDProtoUser[User] {
  def getSingleton = User

  def ownedPackages = Package.findAll(By(Package.owner, this))
}
