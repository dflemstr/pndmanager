package se.dflemstr.pndmanager.util.binary

import scala.xml._
import net.liftweb.http.{FieldError, FieldIdentifier,S}
import net.liftweb.util.{Box,Full,Empty,Failure}

object PXML {
  /** The pattern to search for when searching for the end of a PXML file in a binary */
  final val BinaryStartPattern = "<PXML".getBytes("UTF-8")

  /** The pattern to look for when searching for the end of a PXML file in a binary */
  final val BinaryEndPattern = "</PXML>".getBytes("UTF-8")

  /** Load a PXML from the specified byte array containing PND data */
  def fromPND(data: Array[Byte]): Box[PXML] = fromPND(new PND(data))

  /** Load a PXML from the specified PND */
  def fromPND(pnd: PND): Box[PXML] = try { 
    pnd.PXMLdata.map(x => PXML(x))
  } catch {case ex => Failure(ex.getMessage)}

  /** All available locales on this platform (this is 99% of the time all of the ISO locales) */
  final lazy val availableLocales = java.util.Locale.getAvailableLocales.map(_.toString)
  
  /** Checks if the specified locale string is valid */
  def validLocale(loc: String) = availableLocales.contains(loc)

  /** The UID must match this to qualify as valid */
  val idRegex = """[-_a-z0-9]+"""

  def apply(pnd: PND) = fromPND(pnd)

  /** An empty PXML that lacks entries for all the fields */
  object Empty extends PXML(<PXML></PXML>)
}

/**
 * A simple DOM tool for PXML files
 */
case class PXML(val tree: Elem) {
  require(tree.label == "PXML", S.?("pxml.invalid"))
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
  def validateVersion() = rawVersionStrings.foreach(_ match {
      //fail if the string is empty
      case null | "" => error(S.?("pxml.version.empty"))
      //otherwise, check if it can be converted to an int and is positive
      case s => try { s.toInt >= 0 } catch { 
          case _ => error(S.?("pxml.version.invalid"))
        }
    })

  /** Returns a list of FieldErrors with all the UniqueID issues */
  def validateId() = if (!(id matches idRegex))
    error(S.?("pxml.id.invalid") replace ("%regex%", idRegex))

  /** Returns a list of FieldErrors with all the errors that this PXML has >:-) */
  def validateAll(field: FieldIdentifier) = {
    validateId()
    validateVersion()
  }
  
  /** The unique ID field */
  def id: String = (tree \ "@id").text.replace(".", "_") //TODO: remove this hack if deemed necessary

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
