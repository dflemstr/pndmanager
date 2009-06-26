package se.dflemstr.pndmanager.util.model

import net.liftweb.mapper._
import pndmanager.model._

/** A simple trait that gives a mapper an "owner" field */
trait OwnerField extends BaseLongKeyedMapper {
  object owner extends MappedLongForeignKey[MapperType, Package](this.asInstanceOf[MapperType], Package)
}
