package se.dflemstr.pndmanager.util.openid

import _root_.org.openid4java.discovery.Identifier
import _root_.org.openid4java.consumer._
import _root_.net.liftweb.openid._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http.S
import _root_.scala.xml._

object Vendor extends OpenIdVendor {
  type UserType = Identifier
  type ConsumerType = Consumer[UserType]

  def currentUser = OpenIdUser.is

  def postLogin(id: Box[Identifier],res: VerificationResult): Unit = {
    OpenIdUser(id)
  }

  def logUserOut() {
    OpenIdUser.remove
  }

  def displayUser(in: UserType) = Text(S.?("user.login.succeded") replace ("%user%", in.getIdentifier))

  def createAConsumer = new Consumer[UserType]
}
