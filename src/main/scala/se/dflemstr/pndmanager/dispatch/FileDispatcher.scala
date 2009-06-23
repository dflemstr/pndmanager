package se.dflemstr.pndmanager.dispatch

import util.PXML
import model.Package
import _root_.net.liftweb._
import http._
import util._
import mapper._

object FileDispatcher {
  val dispatcher: PartialFunction[Req, () => Box[LiftResponse]] = {
    //The following suffix hack is require dbecause Lift likes to mess with me
    case Req("package" :: name :: Nil, "pnd", GetRequest) =>
      () => pndFile(name + ".pnd")
    case Req("screenshot" :: id :: Nil, "png", GetRequest) if Package.find(id).isDefined => () => screenshot(id)
    case Req("thumbnail" :: id :: Nil, "png", GetRequest) if Package.find(id).isDefined => () => thumbnail(id)
  }

  protected def makeVersion(strings: List[String]) = {
    val ver = strings.map(_ match {
        case null | "" => 0
        case n => try {n.toInt} catch {case _ => 0}
      })
    (ver(0), ver(1), ver(2), ver(3))
  }

  private def screenshot(id: String): Box[LiftResponse] = {
    val thePackage = Package.find(id).open_!
    Full(InMemoryResponse(thePackage.screenshot, ("Content-Type" -> "image/png") :: Nil, Nil, 200))
  }

  private def thumbnail(id: String): Box[LiftResponse] = {
    val thePackage = Package.find(id).open_!
    Full(InMemoryResponse(thePackage.thumbnail, ("Content-Type" -> "image/png") :: Nil, Nil, 200))
  }

  def pndFile(identifier: String): Box[LiftResponse] = {
    Log.info("A file is being downloaded: " + identifier)
    val masterRegex = "^" + PXML.idRegex + """-(\d+\.){3}\d+\.pnd$"""
    
    if(identifier matches masterRegex) {
      val nameParts = identifier.split("-")
      val versionString = (nameParts.last take (nameParts.last.length - 4)).toString
      val actualName = (nameParts take (nameParts.length - 1)).mkString("")
      val version = makeVersion(versionString.split('.').toList)

      Package.find(
          By(Package.name, actualName),
          By(Package.version, Package.version.valueFromTuple(version))) match {
        case Full(p) =>
          Full(InMemoryResponse(p.pndFile,
                                ("Content-Type" -> "application/x-pandora-pnd") :: Nil, Nil, 200))
        case _ => Empty
      }
    }
    else
      Empty
  }
}
