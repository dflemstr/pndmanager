package se.dflemstr.pndmanager.model

import scala.xml.Text
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object PackageDescription extends PackageDescription with LongKeyedMetaMapper[PackageDescription] {
  override def dbTableName = "descriptions"
}

class PackageDescription extends LocalizedString[PackageDescription] {
  def getSingleton = PackageDescription
  def fieldLength = 4096
}
