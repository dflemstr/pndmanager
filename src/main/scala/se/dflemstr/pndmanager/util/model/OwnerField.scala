package se.dflemstr.pndmanager.util.model

import _root_.net.liftweb.mapper._
import _root_.se.dflemstr.pndmanager.model._

/** A simple trait that gives a mapper an "owner" field */
trait OwnerField extends BaseLongKeyedMapper {
  object owner extends MappedLongForeignKey[MapperType, Package](this.asInstanceOf[MapperType], Package)
}
