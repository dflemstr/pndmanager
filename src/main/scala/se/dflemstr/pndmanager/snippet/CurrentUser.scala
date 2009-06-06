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

  def name(html: NodeSeq): NodeSeq = Text(User.currentUser
         .map(_.niceName) openOr S.?("anonymous"))

  def checkIfNameUnchanged(html: NodeSeq): NodeSeq = {
    val name = User.currentUser.map(_.nickname.is) openOr ""
    val entryparts = S.?("unchanged.nname").split("%usersettingspage%")

    require(entryparts.length == 2, "Error in translation resource: "
            + "unchanged.nname omits the indicator %usersettingspage%")

    if(name.startsWith("change")) S.warning(Text(entryparts(0))
                ++ <lift:menu.item name="EditUser">{S.?("user.settings.page")}</lift:menu.item>
                ++ Text(entryparts(1)))

    html
  }
}
