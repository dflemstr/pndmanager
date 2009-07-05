package se.dflemstr.pndmanager.util.entrysystem

import _root_.net.liftweb.mapper.{Mapper, By, MappedField}
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.rest._
import _root_.scala.xml._

trait RESTApi[T, M <: EntryProvider[T, M]] extends EntryCRD[T, M] with XMLApiHelper {
  this: M =>

  var contentHasChanged: Option[Boolean] = None

  abstract override def creationProcess(mapper: M) = {
    contentHasChanged = Some(true)
    super.creationProcess(mapper)
  }

  abstract override def deletionProcess(mapper: M) = {
    contentHasChanged = Some(true)
    super.deletionProcess(mapper)
  }

  private var cachedDigest: Option[NodeSeq] = None

  def createTag(in: NodeSeq): Elem = <item-api>{in}</item-api>
  def createItem(in: NodeSeq, detailsLink: Boolean, item: M): Elem = <item>{
    in ++ (
      if(detailsLink)
        <details>{"/" + apiNode + "/" + elementAccessNode + "/" + urlFriendlyPrimaryKey(item) + ".xml"}</details>
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
      a.asXML :: Nil
    case _ => Nil
  }).flatMap(x => x), detailsLink, item)

  def dispatch: LiftRules.DispatchPF = {
    case Req(List(`apiNode`, `elementAccessNode`, id), "xml", GetRequest) =>
      () => showItem(id)
    case Req(List(`apiNode`, `digestAccessNode`), "xml", GetRequest) =>
      makeDigestResponse _
  }

  def showItem(item: String): LiftResponse = {
    val e: Box[NodeSeq] =
      for(i <- getSingleton.find(item))
      yield toXML(i, Appearance.Detail, false)
    e
  }

  def makeDigestResponse: LiftResponse =
    nodeSeqToResponse(createDigest)

  def createDigest: NodeSeq = {
    def actuallyCreateDigest() = {
      if(contentHasChanged == Some(true))
        contentHasChanged = Some(false)

      (
        for(i <- getSingleton.findAll)
        yield toXML(i, Appearance.Digest, true)
      ).flatMap(x => x)
    }
    
    (contentHasChanged, cachedDigest) match {
      case (_, None) | (Some(true), _) | (None, _) =>
        //If we don't have anything cached, something has changed,
        //or we don't have information about changes, compute new digest
        val d = actuallyCreateDigest()
        cachedDigest = Some(d)
        d
      case (_, Some(c)) =>
        //Else, if we have cache, use it
        c
    }
  }

}