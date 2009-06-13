package se.dflemstr.pndmanager.model

import net.liftweb.mapper._
import util.{LocalizedString, OwnerField}

object LocalizedPackageDescription extends LocalizedPackageDescription
  with LongKeyedMetaMapper[LocalizedPackageDescription]

class LocalizedPackageDescription extends LocalizedString[LocalizedPackageDescription]
    with OwnerField with IdPK {
  def getSingleton = LocalizedPackageDescription
  val maxLength = 4096 //the max description length
}
