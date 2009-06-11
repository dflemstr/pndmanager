package se.dflemstr.pndmanager.model

import java.util.Date
import java.text.DateFormat
import scala.io.Source
import _root_.java.lang.reflect.Method
import scala.xml._
import net.liftweb._
import sitemap._
import Loc._
import mapper._
import http._
import SHtml._
import util._
import Helpers._

object Package extends Package with LongKeyedMetaMapper[Package] with LongCRUDify[Package] {
  override def fieldOrder = List(name, owner, createdOn, pndFile)
  override def dbTableName = "packages"

  //Give the menu a more suitable name
  override def createMenuName = "Upload new package"

  //Allow only logged-in users to create packages
  override def createMenuLoc = Full(Menu(Loc("Create package", createPath,
    createMenuName, locSnippets, Loc.Template(createTemplate), userLoggedInCondition)))

  override def _createTemplate =
   <lift:crud.create form="post" multipart="true">
      <table id={createId} class={createClass}>
        <crud:field>
          <tr>
            <td>
              <crud:name/>
            </td>
            <td>
              <crud:form/>
            </td>
          </tr>
        </crud:field>

        <tr>
          <td> </td>
          <td><crud:submit>{createButton}</crud:submit></td>
        </tr>
      </table>
   </lift:crud.create>

  //A condition for menus that requires a user to be logged in; otherwise it redirects the visitor to the log in page
  protected def userLoggedInCondition = If(User.loggedIn_? _, () => RedirectResponse(User.loginPageURL))

  def getPXMLFromPND(pnd: Array[Byte]): NodeSeq = try {
    val reversed = pnd.projection.reverse //don't actually create new arrays here, just use a projection
    val reversedStartPattern = "<PXML".getBytes.reverse
    val endString = "</PXML>"

    def cutEnd(pxml: String): String = {
      val parts = pxml.split(endString)
      
      if(parts.length >= 2 || (pxml endsWith "</PXML>"))
        parts(0) + endString
      else error("Could not find end of PXML file")
    }

    //yeah, yeah, this is more CPU intensive than it has to be... I don't care, it's concise
    val candidates = for {
      i <- 0 until Math.min(reversed.length, 32 * 1024).toInt //scan max 32 kb backwards
      candidate = reversed take i
      if(candidate endsWith reversedStartPattern)
    } yield candidate

    val uncutPXML = new String(candidates.last.reverse)
    XML.loadString(cutEnd(uncutPXML))
  } catch { case _ => NodeSeq.Empty }

  override def crudDoForm(item: Package, noticeMsg: String)(in: NodeSeq): NodeSeq = {
    val from = referer
    val snipName = S.currentSnippet

    def loop(html:NodeSeq): NodeSeq = {  
      def flatMapEditable[T](toMap: Package, func: (NodeSeq, Box[NodeSeq], NodeSeq) => Seq[T]): List[T] =
        formFields(toMap)
          .filter(! _.isInstanceOf[NonEditable]) //only allow editable fields
          .flatMap(field => field.toForm.toList.flatMap(fo => func(field.displayHtml, field.fieldId, fo)))

      def doFields(html: NodeSeq): NodeSeq =
        flatMapEditable(item, (title, _, form) =>
          bind("crud", html, "name" -> title, "form" -> form))

      def ifOK(validee: {def validate: List[FieldError]})(action: => Unit) = validee.validate match {
        case Nil => action
        case error =>
          S.error(error)
          snipName.foreach(S.mapSnippet(_, loop))
      }

      def doSubmit() = {
        ifOK(item.pndFile) { //first validate the file
          item.createdOn(new Date)
          item.owner(User.currentUser)
          item.populateFieldsFromPND() //then load things from the file

          ifOK(item) { //then check all other fields
            S.notice(noticeMsg)
            item.save //and save the entry
            S.redirectTo(from)
          }
        }
      }

      bind("crud", html,
           "field" -> doFields _,
           "submit" ->
           ((text: NodeSeq) => SHtml.submit(text.text, doSubmit _)))
    }

    loop(in)
  }
}

trait NonEditable //Just a simple tagging trait that hides a field from editing
trait NotVisibleInSummary

class Package extends LongKeyedMapper[Package] with IdPK {
  import Package.getPXMLFromPND
  
  def getSingleton = Package

  def downloadLoc = Loc("package-" + name.is, List("package", name.is), "Download")

  //A validation for MappedFields for checking if a date is in the future
  private def notInFuture(field: MappedDateTime[Package])(date: Date) = 
    List(FieldError(field, Text(S.?("future.package")))).filter(_ => date.getTime >= System.currentTimeMillis)

  //Converts a date to HTML, automatically using the logged-in user's preferences
  private def dateAsHtml(date: Date) = {
    val formatter = DateFormat.getDateInstance(DateFormat.LONG, S.locale)
    formatter.setTimeZone(S.timeZone)

    scala.xml.Text(formatter.format(date))
  }

  def populateFieldsFromPND() = if(pndFile.dirty_?) {
    val pxml = getPXMLFromPND(pndFile.is)
    name((pxml \ "@id").text) //load name from the ID attribute
    description((pxml \ "description").text)
  }

  //The package owner/maintainer; NOT the author!
  object owner extends MappedLongForeignKey(this, User) with NonEditable {
    override def displayName = S.?("owner")

    override def asHtml = Text(User.findByKey(is).open_!.nickname)
  }

  object createdOn extends MappedDateTime(this) with NonEditable {
    override def displayName = S.?("created")

    override def validations = super.validations ::: List(notInFuture(this) _)

    override def asHtml = dateAsHtml(is)
  }

  object pndFile extends MappedBinary(this) with LifecycleCallbacks {
    def storeTheFile(file: FileParamHolder) =
      this(file.file)

    override def displayName = S.?("pnd.file")

    override def _toForm = Full(SHtml.fileUpload(storeTheFile))
    override def asHtml = <a href={downloadLoc.createLink(NullLocParams)}>{downloadLoc.linkText openOr "Download"}</a>
  }

  object name extends MappedString(this, 64) with NonEditable {
    override def displayName = S.?("name")
    override def validations = onlyAlphaNum _ :: super.validations

    override def dbIndexed_? = true

    def onlyAlphaNum(name: String) = 
      List(FieldError(this, 
                      Text(S.?("invalid.pkgn.only.alphanum") + ": \"" + name + "\""))
      ) filter (_ => !(name matches "[-_a-z0-9\\.]+"))
  }

  object description extends MappedString(this, 2048)
      with NonEditable with NotVisibleInSummary {
    override def displayName = S.?("descr")
  }
}
