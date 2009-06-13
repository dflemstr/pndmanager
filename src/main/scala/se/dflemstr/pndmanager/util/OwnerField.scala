package se.dflemstr.pndmanager.util

import net.liftweb.mapper._
import model._

trait OwnerField extends BaseLongKeyedMapper {
  object owner extends MappedLongForeignKey[MapperType, Package](this.asInstanceOf[MapperType], Package)
}
