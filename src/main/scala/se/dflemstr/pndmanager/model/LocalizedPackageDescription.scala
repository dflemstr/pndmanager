package se.dflemstr.pndmanager.model

import net.liftweb.mapper._
import util.model.{LocalizedString, OwnerField}

/** The MetaMapper for localized package descriptions */
object LocalizedPackageDescription extends LocalizedPackageDescription
  with LongKeyedMetaMapper[LocalizedPackageDescription] {

  override def dbTableName = "pkgdescriptions"
}

/** A database-mapped localized package description */
class LocalizedPackageDescription extends LocalizedString[LocalizedPackageDescription]
    with OwnerField with IdPK {
  def getSingleton = LocalizedPackageDescription
  val maxLength = 4096 //the max description length
}
