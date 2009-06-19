package se.dflemstr.pndmanager.model

import net.liftweb.mapper._
import util.{LocalizedString, OwnerField}

object LocalizedPackageTitle extends LocalizedPackageTitle with LongKeyedMetaMapper[LocalizedPackageTitle] {
  override def dbTableName = "pkgtitles"
}

class LocalizedPackageTitle extends LocalizedString[LocalizedPackageTitle] with OwnerField with IdPK {
  def getSingleton = LocalizedPackageTitle
  val maxLength = 256 //the max title length
}