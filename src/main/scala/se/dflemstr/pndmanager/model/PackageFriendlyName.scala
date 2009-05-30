package se.dflemstr.pndmanager.model


import scala.xml.Text
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

object PackageFriendlyName extends PackageFriendlyName with LongKeyedMetaMapper[PackageFriendlyName] {
  override def dbTableName = "names"
}

class PackageFriendlyName extends LongKeyedMapper[PackageFriendlyName]
    with LocalizedString[PackageFriendlyName] {
  def getSingleton = PackageFriendlyName
  def fieldLength = 128
}