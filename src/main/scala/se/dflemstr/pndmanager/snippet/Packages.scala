package se.dflemstr.pndmanager.snippet

import java.util.Date
import model._
import _root_.net.liftweb.http._
import js.JsCmds
import S._
import _root_.net.liftweb.util._
import Helpers._
import _root_.scala.xml._

class Packages {
  def count: NodeSeq = Text(Package.count.toString)
}
