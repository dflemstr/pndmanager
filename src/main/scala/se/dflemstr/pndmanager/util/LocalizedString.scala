package se.dflemstr.pndmanager.util

import java.util.Locale
import net.liftweb.mapper._

trait LocalizedString[T <: LongKeyedMapper[T]] extends LongKeyedMapper[T] with IdPK {
  this: T =>
  
  val maxLength: Int
  
  object locale extends MappedLocale[T](this)

  object string extends MappedString[T](this, maxLength)
}
