package se.dflemstr.pndmanager.model

import scala.xml.Text
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object User extends User with MetaMegaProtoUser[User] {
  override def dbTableName = "users"
  override val basePath: List[String] = "user" :: Nil
  override def fieldOrder = super.fieldOrder + username + superUser
  override def signupFields = username :: email :: locale :: timezone :: password :: Nil
  
  override def screenWrap = Full(
    <lift:surround with="default" at="content">
        <lift:bind />
    </lift:surround>)

  override def emailDisplayName = S.??("Email address")

  override def niceName: String = (username.is, firstName.is, lastName.is) match {
    case (_, f, l) if f.length > 1 && l.length > 1 => f+" "+l
    case (u, _, _) => u
  }

  override def shortName: String = username.is

  override def skipEmailValidation = true //TODO: change this when we have mail access
}

class User extends MegaProtoUser[User] {
  def getSingleton = User

  object username extends MappedString(this, 64) {
    override def validations = isNameValid _ :: isUnique _ :: super.validations

    override def displayName = S.??("Username")

    def isNameValid(name: String) =
      if(name matches """\w+""") Nil
      else List(FieldError(this, Text(S.??("Invalid username; only letters and numbers allowed."))))

    def isUnique(name: String) =
      if(User.find(name).isEmpty) Nil
      else List(FieldError(this, Text(S.??("Already taken."))))
  }
}
