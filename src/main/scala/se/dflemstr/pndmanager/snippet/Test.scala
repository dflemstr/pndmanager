package se.dflemstr.pndmanager.snippet

import model._
import _root_.net.liftweb.http._
import js.JsCmds
import S._
import _root_.net.liftweb.util._
import Helpers._
import _root_.scala.xml._

class Test {
  def ajax(html: NodeSeq) = SHtml.ajaxButton(Text("Test AJAX spinner"), () => {
    println("Doing delayed AJAX test")
    Thread.sleep(1000)
    JsCmds.Alert("AJAX test done;\nrequest should have taken 1000 ms.")
  })

  def dbDump(html: NodeSeq) = {
    SHtml.ajaxButton(Text("Show a list of all packages"), () => {
      val packages = Package.findAll
      JsCmds.Alert("List of packages:\n" + ("" /: packages) (_ + "\n" + _.name.is))
    })
  }
}
