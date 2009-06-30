package se.dflemstr.pndmanager.model

import _root_.net.liftweb.mapper._
import _root_.se.dflemstr.pndmanager.util.model.{LocalizedString, OwnerField}

/** The MetaMapper for localized package titles */
object LocalizedPackageTitle extends LocalizedPackageTitle with LongKeyedMetaMapper[LocalizedPackageTitle] {
  override def dbTableName = "pkgtitles"
}
/** A database-mapped localized package title */
class LocalizedPackageTitle extends LocalizedString[LocalizedPackageTitle] with OwnerField with IdPK {
  def getSingleton = LocalizedPackageTitle
  val maxLength = 64 //the max title length
}