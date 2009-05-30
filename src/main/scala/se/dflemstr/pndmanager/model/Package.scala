package se.dflemstr.pndmanager.model

import java.util.Date
import java.text.DateFormat
import scala.xml.Text
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object Package extends Package with LongKeyedMetaMapper[Package] with LongCRUDify[Package] {
  override def fieldOrder = List(name, version, owner, creationDate)
  override def dbTableName = "packages"
}

class Package extends LongKeyedMapper[Package] with IdPK {
  def getSingleton = Package

  object owner extends MappedLongForeignKey(this, User) {
    override def displayName = "Owner"
  }

  object name extends MappedString(this, 64) {
    override def displayName = "Name"
    override def validations = onlyAlphaNum _ :: super.validations

    override def dbIndexed_? = true

    def onlyAlphaNum(name: String) =
      if(name matches "[a-zA-Z0-9]+") Nil //no problem
      else List(FieldError(this, Text(S.??("Invalid package name, only alphanumeric names allowed."))))
  }

  object creationDate extends MappedDateTime(this) {
    final val dateFormat = DateFormat.getDateInstance(DateFormat.SHORT)

    override def displayName = "Created"

    override def validations = super.validations ::: List(notInFuture _)

    def notInFuture(date: Date) = try {
        if(date.getTime <= System.currentTimeMillis) Nil
        else List(FieldError(this, Text(S.??("Fatal: Tried to create a package in the future."))))
      } catch {case _ => List(FieldError(this, Text(S.??("Invalid date."))))}

    override def asHtml = scala.xml.Text(dateFormat.format(is))
  }

  object version extends MappedString(this, 32) {
    override def displayName = "Version"
    
    override def validations = startsWithNumber _ :: super.validations

    def startsWithNumber(ver: String) = 
      if(ver matches """\d+.*""") Nil
      else FieldError(this, Text(S.??("The version number must start with a digit!"))) :: Nil
  }
}
