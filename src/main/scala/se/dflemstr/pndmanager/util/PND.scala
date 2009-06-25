package se.dflemstr.pndmanager.util

import scala.xml._
import net.liftweb.util.{Box,Full,Empty,Failure}
import net.liftweb.http.{FieldError, FieldIdentifier, S}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.ByteArrayInputStream

object PND {
  private lazy final val ReversedPXMLStartPattern = PXML.BinaryStartPattern.reverse
  private lazy final val ComputedPXMLStartFailurePattern = util.BinaryTools.computeFailure(ReversedPXMLStartPattern)
  private lazy final val ComputedPXMLEndFailurePattern = util.BinaryTools.computeFailure(PXML.BinaryStartPattern)

  private lazy final val ReversedPNGStartPattern = PNG.BinaryStartPattern.reverse
  private lazy final val ComputedPNGStartFailurePattern = util.BinaryTools.computeFailure(ReversedPNGStartPattern)
}

case class PND(val data: Array[Byte]) {
  import PND._



  /** Returns the PXML data contained within this PND */
  def PXMLdata: Box[Elem] = try {
    val reversed = data.projection.reverse //Use a projection to save memcopies

    //Use the BinaryTools to locate the PXML start; we don't actually gain any speed by using this algorithm but still
    //We use the reverse because it's more likely that we find the PXML at the end
    util.BinaryTools.patternIndex(reversed, ReversedPXMLStartPattern, ComputedPXMLStartFailurePattern) match {
      case Some(reversedStartPos) =>
        val startPos = data.length - (reversedStartPos + ReversedPXMLStartPattern.length)
        val pxmlSlice = data drop startPos
        
        util.BinaryTools.patternIndex(pxmlSlice, PXML.BinaryEndPattern, ComputedPXMLEndFailurePattern) match {
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

    util.BinaryTools.patternIndex(reversed, ReversedPNGStartPattern, ComputedPNGStartFailurePattern) match {
      case Some(reversedStartPos) =>
        val startPos = data.length - (reversedStartPos + ReversedPNGStartPattern.length)
        val png = data drop startPos
        Full(ImageIO.read(new ByteArrayInputStream(png)))
      case None => None
    }
  } catch { case x => Failure(x.getMessage) }
}
