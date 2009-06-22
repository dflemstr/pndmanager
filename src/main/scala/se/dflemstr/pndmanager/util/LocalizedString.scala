package se.dflemstr.pndmanager.util

import java.util.Locale
import net.liftweb.mapper._

/** A "localized string" mapper type, binding a locale and a string together */
trait LocalizedString[T <: LongKeyedMapper[T]] extends LongKeyedMapper[T] with IdPK {
  this: T =>

  /** The charlimit of the string */
  val maxLength: Int

  /** The locale of the string */
  object locale extends MappedLocale[T](this)
  
  /** The localized string itself */
  object string extends MappedPoliteString[T](this, maxLength)
}
