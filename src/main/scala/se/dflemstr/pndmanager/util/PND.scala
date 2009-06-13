package se.dflemstr.pndmanager.util

import scala.xml._

case class PND(val data: Array[Byte]) {
  private lazy final val ReversedStartPattern = PXML.BinaryStartPattern.getBytes.reverse
  private lazy final val ComputedStartFailurePattern = util.BinaryTools.computeFailure(ReversedStartPattern)
  private lazy final val ComputedEndFailurePattern = util.BinaryTools.computeFailure(PXML.BinaryStartPattern)

  def PXMLdata: Elem = {
    val reversed = data.projection.reverse //don't actually create new arrays here, just use a projection

    //Use the BinaryTools to locate the PXML start; we don't actually gain any speed by using this algorithm but meh
    //We use the reverse because it's more likely that we find the PXML at the end
    util.BinaryTools.patternIndex(reversed, ReversedStartPattern, ComputedStartFailurePattern) match {
      case Some(reversedStartPos) =>
        val startPos = data.length - (reversedStartPos + ReversedStartPattern.length)
        val pxmlSlice = data drop startPos
        util.BinaryTools.patternIndex(pxmlSlice, PXML.BinaryEndPattern, ComputedEndFailurePattern) match {
          case Some(endPos) =>
            val pxmlData = pxmlSlice take (endPos + PXML.BinaryEndPattern.length)
            val xml = new String(pxmlData)
            XML.loadString(xml)
          case None => error("The found PXML file does not have an end tag!")
        }
      case None => error("Could not find the beginning of the PXML file!")
    }
  }

  def PNGimage = {
    //TODO: implement
  }
}
