package se.dflemstr.pndmanager.util.binary

import _root_.scala.xml._
import _root_.net.liftweb.util.{Box,Full,Empty,Failure}
import _root_.net.liftweb.http.{FieldError, FieldIdentifier, S}
import _root_.java.awt.image.BufferedImage
import _root_.javax.imageio.ImageIO
import _root_.java.io.ByteArrayInputStream

object PND {
  private lazy final val ReversedPXMLStartPattern = PXML.BinaryStartPattern.reverse
  private lazy final val ComputedPXMLStartFailurePattern = BinaryTools.computeFailure(ReversedPXMLStartPattern)
  private lazy final val ComputedPXMLEndFailurePattern = BinaryTools.computeFailure(PXML.BinaryStartPattern)

  private lazy final val ReversedPNGStartPattern = PNG.BinaryStartPattern.reverse
  private lazy final val ComputedPNGStartFailurePattern = BinaryTools.computeFailure(ReversedPNGStartPattern)
}

case class PND(val data: Array[Byte]) {
  import PND._



  /** Returns the PXML data contained within this PND */
  def PXMLdata: Box[Elem] = try {
    val reversed = data.projection.reverse //Use a projection to save memcopies

    //Use the BinaryTools to locate the PXML start; we don't actually gain any speed by using this algorithm but still
    //We use the reverse because it's more likely that we find the PXML at the end
    BinaryTools.patternIndex(reversed, ReversedPXMLStartPattern, ComputedPXMLStartFailurePattern) match {
      case Some(reversedStartPos) =>
        val startPos = data.length - (reversedStartPos + ReversedPXMLStartPattern.length)
        val pxmlSlice = data drop startPos
        
        BinaryTools.patternIndex(pxmlSlice, PXML.BinaryEndPattern, ComputedPXMLEndFailurePattern) match {
          case Some(endPos) =>
            val pxmlData = pxmlSlice take (endPos + PXML.BinaryEndPattern.length)
            val xml = new String(pxmlData)
            Full(XML.loadString(xml))
          case None => error(S.?("pxml.noendtag"))
        }
      case None => None
    }
  } catch { case x => Failure(x.getMessage) }

  /** Returns the optional PNG screenshot inside of the PND file */
  def PNGdata: Box[BufferedImage] = try {
    val reversed = data.projection.reverse

    BinaryTools.patternIndex(reversed, ReversedPNGStartPattern, ComputedPNGStartFailurePattern) match {
      case Some(reversedStartPos) =>
        val startPos = data.length - (reversedStartPos + ReversedPNGStartPattern.length)
        val png = data drop startPos
        Full(ImageIO.read(new ByteArrayInputStream(png)))
      case None => None
    }
  } catch { case x => Failure(x.getMessage) }
}
