package se.dflemstr.pndmanager.dispatch

import _root_.se.dflemstr.pndmanager.util.binary.PXML
import _root_.se.dflemstr.pndmanager.model.Package
import _root_.net.liftweb._
import _root_.net.liftweb.http._
import _root_.net.liftweb.util._
import _root_.net.liftweb.mapper._
import _root_.java.util.{Date, Locale, TimeZone}
import _root_.java.text.{DateFormat, SimpleDateFormat}

/**
 * Manages files provided by this application
 */
object FileDispatcher {
  /** Creates a HTTP header compatible date */
  def httpDate(date: Date): String = {
    val httpDateFormat = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", Locale.US);
    httpDateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
    httpDateFormat.format(date)
  }

  //Expires after 4 days
  def cacheHeaders =
    "Expires" -> httpDate(new Date(new Date().getTime + 345600000l)) ::
    "Cache-Control" -> "public, max-age=345600" :: Nil

  //Expires after 1 hour
  //This isn't actually used atm; just if we want to dispatch temporary stuff in the future
  def freshHeaders =
    "Expires" -> httpDate(new Date(new Date().getTime + 3600000l)) ::
    "Cache-Control" -> "max-age=3600, must-revalidate" :: Nil

  val dispatcher: PartialFunction[Req, () => Box[LiftResponse]] = {
    //The following suffix hack is required because Lift likes to mess with me
    case Req("package" :: name :: Nil, suffix, GetRequest) =>
      () => pndFile(name + (if(suffix != null && suffix != "") "." + suffix else ""))
    case Req("icon" :: id :: Nil, "png", GetRequest) if Package.find(id).isDefined =>
      () => icon(id)
    case Req("smallicon" :: id :: Nil, "png", GetRequest) if Package.find(id).isDefined =>
      () => smallicon(id)
  }

  protected def makeVersion(strings: List[String]) = {
    val ver = strings.map(_ match {
        case null | "" => 0
        case n => try {n.toInt} catch {case _ => 0}
      })
    (ver(0), ver(1), ver(2), ver(3))
  }

  private def icon(id: String): Box[LiftResponse] = {
    val thePackage = Package.find(id).open_!
    Full(InMemoryResponse(thePackage.icon,
                          ("Content-Type" -> "image/png") :: cacheHeaders, Nil, 200))
  }

  private def smallicon(id: String): Box[LiftResponse] = {
    val thePackage = Package.find(id).open_!
    Full(InMemoryResponse(thePackage.smallicon,
                          ("Content-Type" -> "image/png") :: cacheHeaders, Nil, 200))
  }

  def pndFile(id: String): Box[LiftResponse] = {
    val masterRegex = "^" + PXML.idRegex + """-(\d+\.){3}\d+\.pnd$"""
    
    if(id matches masterRegex) {
      Log.info("A file is being downloaded: " + id) //don't translate!
      val nameParts = id.split("-")
      val versionString = (nameParts.last take (nameParts.last.length - 4)).toString
      val actualName = (nameParts take (nameParts.length - 1)).mkString("-")
      val version = makeVersion(versionString.split('.').toList)

      Package.find(
          By(Package.name, actualName),
          By(Package.version, Package.version.valueFromTuple(version))) match {
        case Full(p) =>
          p.downloadCount(p.downloadCount.is + 1).save
          Full(InMemoryResponse(p.pndFile,
                                ("Content-Type" -> "application/x-pandora-pnd") :: cacheHeaders, Nil, 200))
        case _ => Empty
      }
    }
    else
      Empty
  }
}
