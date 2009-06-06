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
    override def displayName = S.?("owner")
  }

  object name extends MappedString(this, 64) {
    override def displayName = S.?("name")
    override def validations = onlyAlphaNum _ :: super.validations

    override def dbIndexed_? = true

    def onlyAlphaNum(name: String) =
      if(name matches "[a-zA-Z0-9]+") Nil //no problem
      else List(FieldError(this, Text(S.?("invalid.pkgn.only.alphanum"))))
  }

  object creationDate extends MappedDateTime(this) {
    final val dateFormat = DateFormat.getDateInstance(DateFormat.SHORT)

    override def displayName = S.?("created")

    override def validations = super.validations ::: List(notInFuture _)

    def notInFuture(date: Date) = try {
        if(date.getTime <= System.currentTimeMillis) Nil
        else List(FieldError(this, Text(S.?("future.package"))))
      } catch {case _ => List(FieldError(this, Text(S.?("invalid.date"))))}

    override def asHtml = scala.xml.Text(dateFormat.format(is))
  }

  object version extends MappedString(this, 32) {
    override def displayName = S.?("version")
    
    override def validations = startsWithNumber _ :: super.validations

    def startsWithNumber(ver: String) = 
      if(ver matches """\d+.*""") Nil
      else FieldError(this, Text(S.?("version.must.start.digit"))) :: Nil
  }
}
