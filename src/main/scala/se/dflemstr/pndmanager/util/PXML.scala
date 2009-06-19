package se.dflemstr.pndmanager.util

import scala.xml._
import net.liftweb.http.{FieldError, FieldIdentifier}

object PXML {
  /** The pattern to search for when searching for the end of a PXML file in a binary */
  final val BinaryStartPattern = "<PXML"

  /** The pattern to look for when searching for the end of a PXML file in a binary */
  final val BinaryEndPattern = "</PXML>"

  /** Load a PXML from the specified byte array containing PND data */
  def fromPND(data: Array[Byte]): PXML = PXML(new PND(data).PXMLdata)

  /** Load a PXML from the specified PND */
  def fromPND(pnd: PND): PXML = PXML(pnd.PXMLdata)

  /** All available locales on this platform (this is 99% of the time all of the ISO locales) */
  final lazy val availableLocales = java.util.Locale.getAvailableLocales.map(_.toString)
  
  /** Checks if the specified locale string is valid */
  def validLocale(loc: String) = availableLocales.contains(loc)

  /** The UID must match this to qualify as valid */
  val idRegex = """[-_a-z0-9\.]+"""

  /** All the errors that the UID field can have */
  def idFieldErrors(field: FieldIdentifier) = List(FieldError(field,
    Text("The PXML has an invalid unique ID. It must match the regex \"" +
         idRegex + "\"."))) //TODO: translate!

  /** All the errors that the version field can have */
  def versionFieldErrors(field: FieldIdentifier) = List(FieldError(field,
    Text("The PXML has an invalid version number. The version number must " +
         "consist of 4 semi-positive integers, nothing else."))) //TODO: translate!

  def apply(pnd: PND) = fromPND(pnd)

  object Empty extends PXML(<PXML></PXML>)
}

/**
 * A simple DOM tool for PXML files
 */
case class PXML(val tree: Elem) {
  require(tree.label == "PXML", "The loaded tree is not a PXML tree!") //TODO: translate!
  import PXML._

  /** Utility method for field accessors below */
  protected def loadLocalizedStringField(fieldName: String) = (tree \ fieldName)
    .toList.map(descr => (descr.text, (descr \ "@lang").text))
    .filter(tuple => validLocale(tuple._2))

  protected def rawVersionStrings = {
    val versionField = tree \ "version"
    List(versionField\"@major", versionField\"@minor",
      versionField \ "@release", versionField \ "@build").map(_.text)
  }

  /** Returns a list of FieldErrors with all the version field issues */
  def validateVersion(field: FieldIdentifier) = versionFieldErrors(field) filter
    (_ => !rawVersionStrings.forall(_ match {
      //fail if the string is empty
      case null | "" => false
      //otherwise, check if it can be converted to an int and is positive
      case s => try { s.toInt >= 0 } catch { case _ => false }
    }))

  /** Returns a list of FieldErrors with all the UniqueID issues */
  def validateId(field: FieldIdentifier) = idFieldErrors(field) filter (_ => !(id matches idRegex))

  /** Returns a list of FieldErrors with all the errors that this PXML has >:-) */
  def validateAll(field: FieldIdentifier) =
    validateId(field) ::: validateVersion(field)
  
  /** The unique ID field */
  def id: String = (tree \ "@id").text.replace(".", "_") //TODO: remove this hack

  /** A list of localized description fields */
  def description = loadLocalizedStringField("description")

  /** A list of localized title fields */
  def title = loadLocalizedStringField("title")

  /** The version of the PND associated with this PXML */
  def version = {
    val version = rawVersionStrings.map(_ match { //make a nice check here, relative to the validation above
          case null | "" => 0
          case n => try {n.toInt} catch {case _ => 0}
        })
    (version(0), version(1), version(2), version(3))
  }
}
