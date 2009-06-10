package se.dflemstr.pndmanager.model

import java.util.Date
import java.text.DateFormat
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
  override def fieldOrder = List(name, owner, createdOn, pndFile/*, updatedOn*/)
  override def dbTableName = "packages"

  //Give the menu a more suitable name
  override def createMenuName = "Upload new package"

  //Allow only logged-in users to create packages
  override def createMenuLoc = Full(Menu(Loc("Create package", createPath,
    createMenuName, locSnippets, Loc.Template(createTemplate), userLoggedInCondition)))

  //A condition for menus that requires a user to be logged in; otherwise it redirects the visitor to the log in page
  protected def userLoggedInCondition = If(User.loggedIn_? _, () => RedirectResponse(User.loginPageURL))

  override def crudDoForm(item: Package, noticeMsg: String)(in: NodeSeq): NodeSeq = {
    val from = referer
    val snipName = S.currentSnippet

    def loop(html:NodeSeq): NodeSeq = {  
      def flatMapEditable[T](toMap: Package, func: (NodeSeq, Box[NodeSeq], NodeSeq) => Seq[T]): List[T] =
        formFields(toMap)
          .filter(! _.isInstanceOf[NonEditable])
          .flatMap(field => field.toForm.toList.flatMap(fo => func(field.displayHtml, field.fieldId, fo)))

      def doFields(html: NodeSeq): NodeSeq =
        flatMapEditable(item, (title, _, form) =>
          bind("crud", html, "name" -> title, "form" -> form))

      def doSubmit() = item.validate match {
        case Nil =>
          S.notice(noticeMsg)
          item.save
          S.redirectTo(from)

        case xs =>
          S.error(xs)
          snipName.foreach(S.mapSnippet(_, loop))
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

class Package extends LongKeyedMapper[Package] with IdPK {
  def getSingleton = Package

  def downloadLoc = Loc("package" + name.is, List("package", name.is), "Download package")

  //A validation for MappedFields for checking if a date is in the future
  private def notInFuture(field: MappedDateTime[Package])(date: Date) = 
    List(FieldError(field, Text(S.?("future.package")))).filter(_ => date.getTime <= System.currentTimeMillis)

  //Converts a date to HTML, automatically using the logged-in user's preferences
  private def dateAsHtml(date: Date) = {
    val (timezone, locale) = if(User.loggedIn_?) {
      val user = User.currentUser.open_!
      (user.timezone.isAsTimeZone, user.locale.isAsLocale)
    } else (java.util.TimeZone.getDefault, java.util.Locale.getDefault)

    val formatter = DateFormat.getDateInstance(DateFormat.MEDIUM, locale)
    formatter.setTimeZone(timezone)

    scala.xml.Text(formatter.format(date))
  }

  //The package owner/maintainer; NOT the author!
  object owner extends MappedLongForeignKey(this, User) with LifecycleCallbacks with NonEditable {
    override def displayName = S.?("owner")

    override def beforeValidationOnCreate = { this(User.currentUser); super.beforeCreate }

    override def asHtml = Text(User.findByKey(is).open_!.nickname)
  }

  object createdOn extends MappedDateTime(this) with LifecycleCallbacks with NonEditable {
    override def displayName = S.?("created")

    override def beforeValidationOnCreate = {
      this(new Date)
      super.beforeValidationOnCreate
    }

    override def validations = super.validations ::: List(notInFuture(this) _)

    override def asHtml = dateAsHtml(is)
  }

  /*object updatedOn extends MappedDateTime(this) with LifecycleCallbacks with NonEditable {
    override def displayName = S.?("updated")

    override def beforeValidationOnCreate = { 
      this(new Date);
      super.beforeCreate
    }

    override def afterUpdate = { beforeValidationOnCreate; super.beforeUpdate }

    override def validations = super.validations ::: List(notInFuture(this) _)

    override def asHtml = dateAsHtml(is)
  }*/

  object pndFile extends MappedBinary(this) with LifecycleCallbacks {
    def storeTheFile(file: FileParamHolder) = {
      println(file.file)
      this(file.file)
    }

    def autofillSiblingFields() = {
      
    }

    override def beforeValidationOnUpdate {
      validate //This does nothing anyways
      autofillSiblingFields()
    }

    override def displayName = S.?("pnd.file")

    override def _toForm = Full(SHtml.fileUpload(storeTheFile _))
    override def asHtml = <a href={downloadLoc.createLink(NullLocParams)}>{downloadLoc.linkText openOr "Download"}</a>
  }

  object name extends MappedString(this, 64) with NonEditable  with LifecycleCallbacks {
    override def displayName = S.?("name")
    /*override def validations = onlyAlphaNum _ :: super.validations*/

    override def dbIndexed_? = true

    def onlyAlphaNum(name: String) =
      List(FieldError(this, Text(S.?("invalid.pkgn.only.alphanum")))) filter (_ => name matches "[-_a-z0-9]+")
  }
}
