package se.dflemstr.pndmanager.util

import model._
import net.liftweb.mapper._
import net.liftweb._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.http.rest.XMLApiHelper
import scala.xml._

object RestAPI extends XMLApiHelper {
  def createTag(in: NodeSeq): Elem = <pndmanagerapi>{in}</pndmanagerapi>
  def dispatch: LiftRules.DispatchPF = {
    case Req(List("api", "package", pid), "xml", GetRequest) =>
      () => showPackage(pid)
    case Req(List("api", "repository"), "xml", GetRequest) =>
      listPackages _
  }

  def showPackage(packageId: String): LiftResponse = {
    val e: Box[NodeSeq] = 
      for(p <- Package.find(By(Package.id, packageId.toLong)))
      yield p.toXML
    e
  }

  def listPackages(): LiftResponse = {
    val list: NodeSeq = NodeSeq.fromSeq(
      for(p <- Package.findAll)
      yield p.toListXML)
    nodeSeqToResponse(list)
  }
}
