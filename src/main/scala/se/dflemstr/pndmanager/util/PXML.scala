package se.dflemstr.pndmanager.util

import scala.xml._

object PXML {
  final val BinaryStartPattern = "<PXML"
  final val BinaryEndPattern = "</PXML>"

  def fromPND(data: Array[Byte]): PXML = PXML(new PND(data).PXMLdata)
  def fromPND(pnd: PND): PXML = PXML(pnd.PXMLdata)
  
  protected final lazy val availLocales = java.util.Locale.getAvailableLocales.map(_.toString)
  protected def validLocale(loc: String) = availLocales.contains(loc)

  def apply(pnd: PND) = fromPND(pnd)
}

case class PXML(val tree: Elem) {
  require(tree.label == "PXML", "The loaded tree is not a PXML tree!")

  import PXML.validLocale
  
  def id: String = (tree \ "@id").text

  protected def loadLocalizedStringField(fieldName: String) = (tree \ fieldName)
    .toList.map(descr => (descr.text, (descr \ "@lang").text))
    .filter(tuple => validLocale(tuple._2))

  def description = loadLocalizedStringField("description")

  def title = loadLocalizedStringField("title")

  def version = {
    val versionField = tree \ "version"
    val version = List(versionField\"@major", versionField\"@minor",
      versionField \ "@release", versionField \ "@build").map(_.text match {
          case "" => 0
          case n => try {n.toInt} catch {case _ => 0}
        })
    (version(0), version(1), version(2), version(3))
  }
}
