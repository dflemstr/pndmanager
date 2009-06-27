package se.dflemstr.pndmanager.model

import scala.xml.Text
import net.liftweb._
import mapper._
import http._
import js._
import sitemap.{Loc,Menu}
import Loc._
import SHtml._
import util._
import Helpers._
import net.liftweb.openid._
import scala.xml._

import _root_.org.openid4java.message.MessageExtension
import _root_.org.openid4java.message.ax.{AxMessage, FetchResponse}
import _root_.org.openid4java.message.sreg.{SRegMessage, SRegResponse}
import _root_.org.openid4java.discovery.Identifier
import _root_.org.openid4java.consumer._

object User extends User with MetaOpenIDProtoUser[User] {
  /** The database table in which all users are stored */
  override def dbTableName = "users"

  /** The base URL that is used for user management */
  override val basePath: List[String] = "user" :: Nil
  def openIDVendor = pndmanager.util.openid.Vendor

  def findOrCreate(openId: String, verResult: VerificationResult): User =
    find(By(this.openId, openId)) match {
      case Full(u) => u
      case _ =>
        val auth = verResult.getAuthResponse

        def extractFields(extractor: (String) => String): (String, String, String, String) =
          (extractor("email"), extractor("nickname"), extractor("language"), extractor("timezone"))

        def getFields: PartialFunction[MessageExtension, (String, String, String, String)] = {
          case f: FetchResponse => try {
            extractFields(x => f.getAttributeValues(x).get(0).asInstanceOf[String])
          } catch {case _ => (null, null, null, null)}
          case f: SRegResponse => try {
            extractFields(f.getAttributeValue(_))
          } catch {case _ => (null, null, null, null)}
          case _ => (null, null, null, null)
        }

        def wrapField(f: String) = if(f == null || f.length < 1) Empty else Full(f)

        def correctLanguage(l: Box[String]) = l match {
          case Full(l) => 
            val low = l.toLowerCase.replace("-", "_")
            val parts = low.split("_")
            Full(parts(0) + (if(parts.length > 1) "_" + parts(1).toUpperCase else ""))
          case _ => Empty
        }
          
        val (e, n, l, t) =
          if(auth.hasExtension(AxMessage.OPENID_NS_AX)) {
            Log.info("The response had Attribute Extension information")
            getFields(auth.getExtension(AxMessage.OPENID_NS_AX))
          } else if (auth.hasExtension(SRegMessage.OPENID_NS_SREG)) {
            Log.info("The response had SReg information")
            getFields(auth.getExtension(SRegMessage.OPENID_NS_SREG))
          } else (null, null, null, null)

        val nickname = wrapField(n)
        val email = wrapField(e)
        val language = correctLanguage(wrapField(l))
        val timezone = wrapField(t)

        val nada = "Nothing"
        Log.info("Creating user, with nick: '" + (nickname openOr nada) +
                 "' email: '" + (email openOr nada) +
                 "' lang: '" + (language openOr nada) +
                 "' tzone: '" + (timezone openOr nada) + "'")

        def randomNick = "change" + Helpers.randomInt(1000000000)
        def randomEmail = Helpers.randomInt(100000000) + "unknown@unknown.com"

        val newUser = create
          .openId(openId)
          .nickname(nickname openOr randomNick)
          .password(Helpers.randomString(15))
          .locale(language)
          .timezone(timezone)
          .email(email openOr randomEmail)
          
        if(newUser.validate == Nil)
          newUser.saveMe
        else {
          newUser
            .nickname(randomNick)
            .email(randomEmail)
            .locale(Empty)
            .timezone(Empty)
            .saveMe
        }
  }

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
      (openid, exp, fo) match {
        case (Full(id), _, Full(f)) =>
          val user = this.findOrCreate(id.getIdentifier, f)
          logUserIn(user)
          S.notice(S.?("user.login.succeded") replace ("%user%", user.niceName))
        case (_, Full(exp), Full(_)) =>
          S.error(S.?("user.login.exception") replace ("%exception%", exp.getMessage))
        case (_, _, Full(f)) =>
          S.error(S.?("user.login.failure") replace ("%reason%", f.getStatusMsg))
        case _ => S.error(S.?("user.login.notfound"))
      }
      RedirectResponse(homePage)
    }

    bind("user", xhtml,
         "openid" -> (js.JsCmds.FocusOnLoad(<input type="text" name="username"/>)),
         "submit" -> (<input type="submit" value={S.??("log.in")}/>))
  }

  /** Create an edit form with the specified template */
  def edit(xhtml: NodeSeq): NodeSeq = {
    val theUser: User = currentUser.open_! // we know we're logged in

    def testEdit() {
      theUser.validate match {
        case Nil =>
          if(Props.get("pndmanager.adminuname") == theUser.nickname.is)
            theUser.superUser(true)
          theUser.save
          S.notice(S.??("profile.updated"))
          S.redirectTo(S.referer openOr "/")

        case xs =>
          S.error(xs)
          editFunc(Full(innerEdit _))
      }
    }

    def innerEdit = bind("user", xhtml,
      "field" -> ((x: NodeSeq) => localForm(x, theUser, true)),
      "submit" -> ((_: NodeSeq) => SHtml.submit(S.??("edit"), testEdit _)))

    editFunc.map(_()) openOr innerEdit
  }

  /* Make a form with all the editable fields of an user, from a template */
  protected def localForm(xhtml: NodeSeq, user: User, ignorePassword: Boolean): NodeSeq = {
    signupFields
      .map(fi => getSingleton.getActualBaseField(user, fi)) //get actual fields
      .filter(f => !ignorePassword || (f match { //remove the password field
        case _: MappedPassword[_] => false
        case _ => true
      }))
      .flatMap(field => 
          field.toForm.toList.flatMap(form =>
            bind("field", xhtml,
              "name" ->(if(field.displayName == "nickname")
                          Text("Nickname")
                        else
                          Text(field.displayName)),
              "form" -> form)
          )
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
    Full(Menu(Loc("user.login", loginPath, S.??("log.in"),
                  If(User.notLoggedIn_? _, S.??("edit.profile")))))

  /**
   * The menu item for logout
   */
  override def logoutMenuLoc: Box[Menu] =
    Full(Menu(Loc("user.logout", logoutPath, S.??("log.out"), testLogginIn)))

  /**
   * The menu item for editing the user
   */
  override def editUserMenuLoc: Box[Menu] =
    Full(Menu(Loc("user.edit", editPath, S.??("edit.profile"), testLogginIn)))

  override def createUserMenuLoc: Box[Menu] =  Empty
  override def lostPasswordMenuLoc: Box[Menu] = Empty
  override def resetPasswordMenuLoc: Box[Menu] = Empty
  override def changePasswordMenuLoc: Box[Menu] = Empty

  //don't want to ruin the staircase effect, leaving space here ;)

  override def validateUserMenuLoc: Box[Menu] = Empty

  def adminListMenu: Box[Menu] = Full(Menu(Loc("useradmin.list", List("useradmin", "list"), "Manage Users", Snippet("UserAdmin.list", makeAdminList _))))

  def makeAdminList(template: NodeSeq): NodeSeq = {
    def makeSU(u: User): NodeSeq = {
      val updateID = "su-" + u.id.is
      <div id={updateID}>{
        if(!(User.currentUser.map(_.superUser.is) openOr false))
          Text("You're not allowed to see this!")
        else {
          if(u.nickname.is != (Props.get("pndmanager.adminuname") openOr "")) {
            SHtml.ajaxCheckbox(u.superUser.is, x => {
              u.superUser(x).save
              JsCmds.SetHtml(updateID, makeSU(u))
            })
          }
          else
            Text(":-P")
        }
      }</div>
    }

    if(!(User.currentUser.map(_.superUser.is) openOr false))
      Text("You're not allowed to see this!")
    else {
      User.findAll.map(u => 
        bind("user", template,
             "name" -> Text(u.niceName),
             "superuser" -> makeSU(u),
             "delete" -> SHtml.a(() => {u.delete_!; JsCmds.Noop}, Text("Delete")))
      ).flatMap(x => x)
    }
  }
}

class User extends OpenIDProtoUser[User] {
  def getSingleton = User

  def ownedPackages = Package.findAll(By(Package.owner, this))

  override def niceName: String = (firstName.is, lastName.is, nickname.is) match {
    case (f, l, n) if f.length > 1 && l.length > 1 => f + " \"" + n + "\" " + l
    case (f, _, n) if f.length > 1 => f + " (" + n + ")"
    case (_, l, n) if l.length > 1 => "\"" + n + "\"" + l
    case (_, _, n) => n
  }

  override def niceNameWEmailLink = <xml:group/>
}