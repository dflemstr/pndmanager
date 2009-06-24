package se.dflemstr.pndmanager.model

import net.liftweb.mapper._
import util.{LocalizedString, OwnerField}

/** The MetaMapper for localized package titles */
object LocalizedPackageTitle extends LocalizedPackageTitle with LongKeyedMetaMapper[LocalizedPackageTitle] {
  override def dbTableName = "pkgtitles"
}
/** A database-mapped localized package title */
class LocalizedPackageTitle extends LocalizedString[LocalizedPackageTitle] with OwnerField with IdPK {
  def getSingleton = LocalizedPackageTitle
  val maxLength = 64 //the max title length
}