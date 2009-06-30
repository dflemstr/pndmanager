package se.dflemstr.pndmanager.model

import _root_.java.util.Date
import _root_.java.text.DateFormat
import _root_.net.liftweb._
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.SHtml._
import _root_.net.liftweb.util._
import _root_.scala.xml._

/** The MetaMapper for categories */
object Category extends Category with LongKeyedMetaMapper[Category] with LongCRUDify[Category] {
  override def dbTableName = "categories"
}

/** A database-mapped package category */
class Category extends LongKeyedMapper[Category] with IdPK {
  def getSingleton = Category

  /** The name of this category */
  object name extends MappedString(this, 32) {
    override def displayHtml = Text("Name")
  }
}
