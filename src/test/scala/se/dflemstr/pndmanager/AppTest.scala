package se.dflemstr.pndmanager

import _root_.org.specs._
import _root_.org.specs.runner._
import _root_.java.io.File
import _root_.scala.xml.XML
import _root_.net.liftweb.util._

class AppTest extends Runner(App) with JUnit with Console

object App extends Specification {
  "The PND manager application" should {
    "contain only valid XML files" >> {
      var failed: List[File] = Nil

      def handledXml(file: String) =
        file.endsWith(".xml")

      def handledXHtml(file: String) =
        file.endsWith(".html") || file.endsWith(".htm") || file.endsWith(".xhtml")

      def wellFormed(file: File) {
        if (file.isDirectory)
          for (f <- file.listFiles) wellFormed(f)

        if (file.isFile && handledXml(file.getName)) {
          try {
            XML.loadFile(file)
          } catch {
            case e: _root_.org.xml.sax.SAXParseException => failed = file :: failed
          }
        }
        if (file.isFile && handledXHtml(file.getName)) {
          PCDataXmlParser(new java.io.FileInputStream(file.getAbsolutePath)) match {
            case Full(_) => // file is ok
            case _ => failed = file :: failed
          }
        }
      }

      wellFormed(new File("src/main/webapp"))

      val numFails = failed.size
      if (numFails > 0) {
        val fileStr = if (numFails == 1) "file" else "files"
        val msg = "Malformed XML in " + numFails + " " + fileStr + ": " + failed.mkString(", ")
        fail(msg)
      }
    }
  }
}
