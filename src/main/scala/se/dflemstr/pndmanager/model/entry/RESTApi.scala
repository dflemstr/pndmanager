package se.dflemstr.pndmanager.model.entry

import _root_.net.liftweb.mapper.{Mapper, By, MappedField}
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.rest._
import _root_.scala.xml._

trait RESTApi[T, M <: EntryProvider[T, M]] extends EntryCRD[T, M] with XMLApiHelper {
  this: M =>

  def createTag(in: NodeSeq): Elem = <item-api>{in}</item-api>

  lazy val apiNode = "api"
  lazy val elementAccessNode = "item"
  lazy val digestAccessNode = "digest"

  private def toXML(item: M, appearance: Appearance.Value): NodeSeq =
    item.entries.map(_ match {
      case a: APIExposed[_] => List(a.asXML) ++ Text("\n")
      case _ => Nil
    }).flatMap(x => x)

  def dispatch: LiftRules.DispatchPF = {
    case Req(List(`apiNode`, `elementAccessNode`, id), "xml", GetRequest) =>
      () => showItem(id)
    case Req(List(`apiNode`, `digestAccessNode`), "xml", GetRequest) =>
      createDigest _
  }

  def showItem(item: String): LiftResponse = {
    val e: Box[NodeSeq] =
      for(i <- getSingleton.find(item))
      yield toXML(i, Appearance.Detail)
    e
  }

  def createDigest: LiftResponse = {
    val list: NodeSeq =
      (for(i <- getSingleton.findAll)
      yield
        <details>{S.contextPath + "/api/" + elementAccessNode + "/" + urlFriendlyPrimaryKey(i) + ".xml"}</details> ++
        toXML(i, Appearance.Digest)
      ).flatMap(x => x)
    nodeSeqToResponse(list)
  }

}