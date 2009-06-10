package se.dflemstr.pndmanager.model

import scala.xml.Text
import net.liftweb._
import mapper._
import http._
import js._
import JsCmds._
import SHtml._
import util._
import Helpers._
import net.liftweb.openid._
import scala.xml._

import _root_.org.openid4java.discovery.Identifier
import _root_.org.openid4java.consumer._

object User extends User with MetaOpenIDProtoUser[User] {
  override def dbTableName = "users"
  def openIDVendor = SimpleOpenIdVendor
  override val basePath: List[String] = "user" :: Nil
  
  override def screenWrap = Full(
    <lift:surround with="default" at="content">
        <lift:bind />
    </lift:surround>)

  override def loginXhtml =
  <form method="post" action={S.uri}>
    <table>
      <tr>
        <td>{S.?("your")} <lift:OpenID.link>OpenID</lift:OpenID.link>:</td>
        <td><user:openid /></td>
      </tr>
      <tr>
        <td>&nbsp;</td>
        <td>{S.?("openid.name.explanation")}</td>
      </tr>
      <tr>
        <td>&nbsp;</td><td><user:submit /></td>
      </tr>
    </table>
  </form>

  override def editXhtml(user: User) =
  <form method="post" action={S.uri}>
     <table>
       {localForm(user, true)}
       <tr><td> </td><td><user:submit/></td></tr>
     </table>
  </form>

  override def login = {
    if (S.post_?) {
      S.param("username").
      foreach(username =>
              openIDVendor.loginAndRedirect(username, performLogUserIn)
      )
    }
    def performLogUserIn(openid: Box[Identifier],
                                  fo: Box[VerificationResult],
                                  exp: Box[Exception]): LiftResponse = {
      (openid, exp) match {
        case (Full(id), _) =>
          val user = this.findOrCreate(id.getIdentifier)
          logUserIn(user)
          S.notice(S.?("welcome.user").replace("%user%", user.niceName))
        case (_, Full(exp)) =>
          S.error(S.?("report.exception").replace("%exception%", exp.getMessage))
        case _ =>
          S.error(S.?("report.loginreason").replace("%reason%", fo.toString))
      }
      RedirectResponse("/")
    }

    Helpers.bind("user", loginXhtml,
                 "openid" -> (FocusOnLoad(<input type="text" name="username"/>)),
                 "submit" -> (<input type="submit" value={S.??("log.in")}/>))
  }

  protected def localForm(user: User, ignorePassword: Boolean): NodeSeq = {
    signupFields.
    map(fi => getSingleton.getActualBaseField(user, fi)).
    filter(f => !ignorePassword || (f match {
          case f: MappedPassword[User] => false
          case _ => true
        })).
    flatMap(f =>
      f.toForm.toList.map(form => //hack the nickname until it is translatable "for real", upstream
        (<tr>
            <td>{if(f.displayName == "nickname") S.?("nickname") else f.displayName}</td>
            <td>{form}</td>
         </tr>) ) )
  }

  override def findByNickname(str: String): List[User] =
    findAll(By(nickname, str))
}

class User extends OpenIDProtoUser[User] {
  def getSingleton = User

  def ownedPackages = Package.findAll(By(Package.owner, this))
}
