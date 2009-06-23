package se.dflemstr.pndmanager.model

import java.util.Date
import java.text.DateFormat
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._
import scala.xml._

object Category extends Category with LongKeyedMetaMapper[Category] {
  override def dbTableName = "categories"
}

class Category extends LongKeyedMapper[Category] with IdPK {
  def getSingleton = Category

  object name extends MappedString(this, 32)
}
