package se.dflemstr.pndmanager.util.entrysystem

import _root_.net.liftweb.mapper.{Mapper, By, MappedField}
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.rest._
import _root_.scala.xml._

trait RESTApi[T, M <: EntryProvider[T, M]] extends EntryCRD[T, M] with XMLApiHelper {
  this: M =>

  def createTag(in: NodeSeq): Elem = <item-api>{in}</item-api>
  def createItem(in: NodeSeq, detailsLink: Boolean, item: M): Elem = <item>{
    in ++ (
      if(detailsLink)
        <details>{S + "/api/" + elementAccessNode + "/" + urlFriendlyPrimaryKey(item) + ".xml"}</details>
      else
        Nil
      )
  }</item>

  lazy val apiNode = "api"
  lazy val elementAccessNode = "item"
  lazy val digestAccessNode = "digest"

  private def toXML(item: M, appearance: Appearance.Value, detailsLink: Boolean): NodeSeq =
  createItem(item.entries.map(_ match {
    case a: APIExposed[_] if(a.isVisibleIn(appearance)) =>
      Text("    ") :: a.asXML :: Text("\n") :: Nil
    case _ => Nil
  }).flatMap(x => x), detailsLink, item)

  def dispatch: LiftRules.DispatchPF = {
    case Req(List(`apiNode`, `elementAccessNode`, id), "xml", GetRequest) =>
      () => showItem(id)
    case Req(List(`apiNode`, `digestAccessNode`), "xml", GetRequest) =>
      createDigest _
  }

  def showItem(item: String): LiftResponse = {
    val e: Box[NodeSeq] =
      for(i <- getSingleton.find(item))
      yield toXML(i, Appearance.Detail, false)
    e
  }

  def createDigest: LiftResponse = {
    val list: NodeSeq =
      (for(i <- getSingleton.findAll)
      yield
        toXML(i, Appearance.Digest, true)
      ).flatMap(x => x)
    nodeSeqToResponse(list)
  }

}