package se.dflemstr.pndmanager.model

import java.util.Date
import java.text.DateFormat
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._
import scala.xml._

/** The MetaMapper for categories */
object Category extends Category with LongKeyedMetaMapper[Category] {
  override def dbTableName = "categories"
}

/** A database-mapped package category */
class Category extends LongKeyedMapper[Category] with IdPK {
  def getSingleton = Category

  /** The name of this category */
  object name extends MappedString(this, 32)
}
