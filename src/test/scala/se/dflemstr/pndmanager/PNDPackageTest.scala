package se.dflemstr.pndmanager

import _root_.org.specs._
import _root_.org.specs.runner._
import model._
import util._
import scala.xml._

class PNDPackageTest extends Runner(PNDPackage) with JUnit with Console

object PNDPackage extends Specification {
  "The Package singleton object" should {

    "be able to extract a PXML from the end of a PND" in {
      val pnd = "jdhkfsdjfhlk\uafeee<PXML><title lang=\"en_US\">Sample App 1</title></PXML>".getBytes
      val pxml = PND(pnd).PXMLdata

      pxml.open_! must_== (<PXML><title lang="en_US">Sample App 1</title></PXML>)
    }

    "manage to remove trailing trash from an extracted PXML file" in {
      val pnd = "jskld\u0000ajfölska\u2341jdöflkj<PXML><title lang=\"en_US\">Sample App 2</title></PXML>jkjhsdkajhsl\u2231kewyutuqiytuiytir".getBytes
      val pxml = PND(pnd).PXMLdata

      pxml.open_! must_== (<PXML><title lang="en_US">Sample App 2</title></PXML>)
    }
  }
}
