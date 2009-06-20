package se.dflemstr.pndmanager.util

import scala.xml._
import net.liftweb.util.{Box,Full,Empty,Failure}
import net.liftweb.http.{FieldError, FieldIdentifier}

object PND {
  private lazy final val ReversedStartPattern = PXML.BinaryStartPattern.reverse
  private lazy final val ComputedStartFailurePattern = util.BinaryTools.computeFailure(ReversedStartPattern)
  private lazy final val ComputedEndFailurePattern = util.BinaryTools.computeFailure(PXML.BinaryStartPattern)
}

case class PND(val data: Array[Byte]) {
  import PND._

  /** Returns the PXML data contained within this PND */
  def PXMLdata: Box[Elem] = try {
    val reversed = data.projection.reverse //Use a projection to save memcopies

    //Use the BinaryTools to locate the PXML start; we don't actually gain any speed by using this algorithm but still
    //We use the reverse because it's more likely that we find the PXML at the end
    util.BinaryTools.patternIndex(reversed, ReversedStartPattern, ComputedStartFailurePattern) match {
      case Some(reversedStartPos) =>
        val startPos = data.length - (reversedStartPos + ReversedStartPattern.length)
        val pxmlSlice = data drop startPos
        
        util.BinaryTools.patternIndex(pxmlSlice, PXML.BinaryEndPattern, ComputedEndFailurePattern) match {
          case Some(endPos) =>
            val pxmlData = pxmlSlice take (endPos + PXML.BinaryEndPattern.length)
            val xml = new String(pxmlData)
            Full(XML.loadString(xml))
          case None => error("The found PXML file in the PND does not have an end tag!") //TODO: translate!
        }
      case None => error("Could not find the beginning of the PXML file inside of the PND!") //TODO: translate!
    }
  } catch { case x => Failure(x.getMessage) }

  /** Returns the optional PNG screenshot inside of the PND file */
  def PNGimage = {
    //TODO: implement
  }
}
