package se.dflemstr.pndmanager.model

import scala.xml.Text
import net.liftweb._
import mapper._
import http._
import SHtml._
import util._

trait LocalizedString[T <: LocalizedString[T]] extends LongKeyedMapper[T] with IdPK {
  this: T =>
  //abstract:
  def fieldLength: Int

  object locale extends MappedLocale[T](this)
  object data extends MappedPoliteString[T](this, fieldLength)

  object container extends MappedLongForeignKey[T, Package](this, Package)
}