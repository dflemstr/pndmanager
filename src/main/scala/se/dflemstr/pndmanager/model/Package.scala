package se.dflemstr.pndmanager.model

import _root_.java.util.{Date, Locale}
import _root_.java.text.DateFormat

import _root_.scala.xml._

import _root_.net.liftweb._
import sitemap._
import Loc._
import mapper._
import http._
import util.Helpers._
import util.{Box, Full, Log}

import _root_.se.dflemstr.pndmanager.util._

object Package extends Package with LongKeyedMetaMapper[Package] with LongCRUDify[Package] {
  override def fieldOrder = List(name, version, owner, createdOn, pndFile)
  override def dbTableName = "packages"

  //Give the menu a more suitable name
  override def createMenuName = "Upload new package"

  //Allow only logged-in users to create packages
  override def createMenuLoc = Full(Menu(Loc("Create package", createPath,
    createMenuName, locSnippets, Loc.Template(createTemplate), userAuthorization)))

  override def _createTemplate =
   <lift:crud.create form="post" multipart="true">
      <table id={createId} class={createClass}>
        <tbody>
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
        </tbody>
        <tfoot>
          <tr>
            <td> </td>
            <td><crud:submit>{createButton}</crud:submit></td>
          </tr>
        </tfoot>
      </table>
   </lift:crud.create>

  //A condition for menus that requires a user to be logged in; otherwise it redirects the visitor to the log in page
  protected def userAuthorization = If(User.loggedIn_? _, () => RedirectResponse(User.loginPageURL))

  //Check if the current user is allowed to change a package
  protected def mutablePackage_?(p: Package) =
    User.loggedIn_? &&
      (p.owner.is == User.currentUser || (User.currentUser.map(_.superUser.is) openOr false))

  override def crudDoForm(item: Package, noticeMsg: String)(in: NodeSeq): NodeSeq = {
    val from = referer
    val snipName = S.currentSnippet

    def loop(html:NodeSeq): NodeSeq = {  
      def flatMapEditable[T](toMap: Package, func: (NodeSeq, Box[NodeSeq], NodeSeq) => Seq[T]): List[T] =
        formFields(toMap)
          .filter(_.isInstanceOf[Editable]) //only allow editable fields
          .flatMap(field => field.toForm.toList.flatMap(fo => func(field.displayHtml, field.fieldId, fo)))

      def doFields(html: NodeSeq): NodeSeq =
        flatMapEditable(item, (title, _, form) =>
          bind("crud", html, "name" -> title, "form" -> form))
        
      def ifOK(validee: {def validate: List[FieldError]})(action: => Unit)(other: => Unit) = validee.validate match {
        case Nil => action
        case error =>
          S.error(error)
          try { other } catch {case _ => }
          snipName.foreach(S.mapSnippet(_, loop))
      }

      def doSubmit() = {
        ifOK(item.pndFile) { //first validate the file
          item.createdOn(new Date).owner(User.currentUser)
          ifOK(item.populateFieldsFromPND()) { //then load things from the file
            ifOK(item) { //then check all other fields
              S.notice(noticeMsg)
              item.save //and save the entry
              S.redirectTo(from)
            } /*else*/ {item.delete_!}
          } /*else*/ {item.delete_!}
        } /*else*/ {item.delete_!}
      }

      bind("crud", html,
           "field" -> doFields _,
           "submit" ->
           ((text: NodeSeq) => SHtml.submit(text.text, doSubmit _)))
    }

    loop(in)
  }

  override lazy val locSnippets = new DispatchLocSnippets {
    val dispatch: PartialFunction[String, NodeSeq => NodeSeq] = {
      case "crud.all" => doCrudAll
      case "crud.create" => crudDoForm(create, S.??("Created"))
    }

    def doCrudAll(in: NodeSeq): NodeSeq = {
      val first = S.param("first").map(toLong) openOr 0L
      val list = findForList(first, 21)

      def prev(in: NodeSeq) = if (first < 21) <xml:group>&nbsp;</xml:group>
      else <a href={listPathString+"?first="+(0L max (first - 20L))}>{in}</a>

      def next(in: NodeSeq) = if (list.length < 21) <xml:group>&nbsp;</xml:group>
      else <a href={listPathString+"?first="+(first + 20L)}>{in}</a>

      def filter(entries: List[Visible]) =
        entries.filter(_.scope match {
          case Summary => true
          case Digest => true
          case Custom(cond) => cond()
          case Detail => false
        })

      def doHeaderItems(in: NodeSeq): NodeSeq = filter(visibleEntries)
        .flatMap(f => bind("crud", in, "name" -> f.displayHtml))

      def doRows(in: NodeSeq): NodeSeq =
      list.take(20).flatMap {
        c =>
        def doRowItem(in: NodeSeq): NodeSeq = filter(c.visibleEntries)
          .flatMap(f => bind("crud", in, "value" -> f.asHtml))

        bind("crud", in , "row_item" -> doRowItem _,
          FuncAttrBindParam("edit_href", _ =>
            if(mutablePackage_?(c))
              Text(editPathString+"/"+(obscurePrimaryKey(c)))
            else
              Text(""),
            "href"),

          FuncAttrBindParam("view_href", _ =>
            Text(viewPathString+"/"+
                 (obscurePrimaryKey(c))),
            "href"),

          FuncAttrBindParam("delete_href", _ =>
            if(mutablePackage_?(c))
              Text(deletePathString+"/"+(obscurePrimaryKey(c)))
            else 
              Text(""),
            "href")
        )}

      bind("crud", in, "header_item" -> doHeaderItems _,
           "row" -> doRows _,
           "prev" -> prev _, "next" -> next _)

    }
  }
}

trait Editable //Just a simple tagging trait that shows fields when editing

trait Visible {
  def asHtml: NodeSeq
  def displayHtml: NodeSeq
  val scope: VisibleScope
}

/** A class that provides information about where a field is displayed */
sealed case class VisibleScope

/** Display the field in digests, summaries/lists and detail views */
case object Digest extends VisibleScope

/** Display the field in summaries/lists and detail views */
case object Summary extends VisibleScope

/** Display the field only in detail views */
case object Detail extends VisibleScope

/** Use a custom method for determining if the field is visible */
case class Custom(val isVisible: () => Boolean) extends VisibleScope

class Package extends LongKeyedMapper[Package] with IdPK {
  def getSingleton = Package
  def visibleEntries: List[Visible] =
    List(title, name, version, description, owner, createdOn, pndFile)
    
  def downloadLoc = Loc("package-" + name.is,
                        List("package", name.is + "-" + version.is + ".pnd"), "Download")

  //The package owner/maintainer; NOT the author!
  object owner extends MappedLongForeignKey(this, User) with Visible {
    val scope = Summary

    override def displayName = S.?("owner")

    override def asHtml = Text(User.findByKey(is).map(_.nickname.is) openOr "Unknown")
  }

  object createdOn extends MappedDateTime(this) with Visible {
    val scope = Detail
    
    override def displayName = S.?("created")

    override def validations = super.validations ::: List(notInFuture(this) _)

    override def asHtml = dateAsHtml(is)
  }

  object pndFile extends MappedBinary(this) with Editable with Visible {
    val scope = Digest

    override def validations = notZeroSize _ :: containsPXML _ :: super.validations

    def notZeroSize(data: Array[Byte]) =
      List(FieldError(this, Text("The PND file didn't contain anything at all!")))
        .filter(_ => data.length == 0)

    def containsPXML(data: Array[Byte]) =
      List(FieldError(this, Text("The specified PND does not contain PXML information!")))
        .filter(_ => try {PXML.fromPND(data).tree == <PXML></PXML>} //TODO: the PXML is read twice, fix this!
                     catch { case _ => true}) //if the PXML is empty or couldn't be found

    def storeTheFile(file: FileParamHolder) =
      set(file.file) //store the binary information of the file

    override def displayName = S.?("pnd.file")

    override def _toForm = Full(SHtml.fileUpload(storeTheFile))
    override def asHtml = <a href={downloadLoc.createLink(NullLocParams)}>{downloadLoc.linkText openOr "Download"}</a>
  }

  object name extends MappedString(this, 64) with Visible {
    val scope = Digest

    override def displayName = S.?("name")
    override def validations = onlyAlphaNum _ :: super.validations

    override def dbIndexed_? = true

    def onlyAlphaNum(name: String) = 
      List(FieldError(this, 
                      Text(S.?("invalid.pkgn.only.alphanum") + ": \"" + name + "\""))
      ) filter (_ => !(name matches "[-_a-z0-9\\.]+"))
  }

  object version extends MappedString(this, 32) with Visible {
    //I'm too lazy to make a new mapped field and this solution has its benefits anyways
    //So MappedString suffices for now
    val scope = Digest

    override def displayName = "Version"

    protected def pad(value: String) =
      "0" * (8 - value.length) + value

    def fromTuple(version: (Int, Int, Int, Int)) = {
      def toHex(i: Int) = pad(Integer.toHexString(i))
      set(toHex(version._1) + toHex(version._2) + toHex(version._3) + toHex(version._4))
    }
    
    protected def verList(s: String): List[Int] = s match {
      case "" => Nil
      case _ =>
        Integer.parseInt(s take 8, 16) :: verList(s drop 8)
    }

    def toHumanReadable = {
      verList(is).mkString(".")
    }

    def toTuple: (Int, Int, Int, Int) = {
      val list = verList(is)
      (list(0), list(1), list(2), list(3))
    }

    override def asHtml = Text(toHumanReadable)
  }

  object description extends Visible {
    val scope = Detail

    def displayHtml = Text("Description")

    def asHtml = {
      val descriptions = LocalizedPackageDescription.findAll(
        By(LocalizedPackageDescription.owner, Package.this))
      
      val localized = descriptions.find(_.locale == S.locale)
      val en_US = descriptions.find(_.locale == Locale.US)

      (localized, en_US) match {
        case (Some(descr), _) => Text(descr.string)
        case (None, Some(descr)) => Text(descr.string)
        case (None, None) => <em>(No translation found for {S.locale})</em>
      }
    }
  }

  object title extends Visible {
    val scope = Summary

    def displayHtml = Text("Title")

    def asHtml = { //TODO: eliminate this code duplication
      val titles = LocalizedPackageTitle.findAll(
        By(LocalizedPackageTitle.owner, Package.this))
      Log.info(titles.length.toString)

      titles.foreach(x => {Log.info(x.locale); Log.info(x.owner); Log.info(x.string)})

      val localized = titles.find(_.locale == S.locale)
      val en_US = titles.find(t => t.locale == "en_US" || t.locale == "en")

      (localized, en_US) match {
        case (Some(title), _) => Text(title.string)
        case (None, Some(title)) => Text(title.string)
        case (None, None) => <em>(No translation found for {S.locale})</em>
      }
    }
  }

  //A validation for MappedFields for checking if a date is in the future
  protected def notInFuture(field: MappedDateTime[Package])(date: Date) =
    List(FieldError(field, Text(S.?("future.package"))))
      .filter(_ => date.getTime >= System.currentTimeMillis)

  //Converts a date to HTML, automatically using the logged-in user's preferences
  protected def dateAsHtml(date: Date) = {
    val formatter = DateFormat.getDateInstance(DateFormat.LONG, S.locale)
    formatter.setTimeZone(S.timeZone)

    scala.xml.Text(formatter.format(date))
  }

  /**
   * Reads the pndFile field and populates all the other fields and entries with
   * acquired information. pndFile must be validated before this can happen!
   */
  def populateFieldsFromPND(): {def validate: List[FieldError]} = {
    Package.this.save //just so that we get an ID; neccessary for associating localized strings
    import scala.collection.mutable
    val errors = new mutable.ListBuffer[FieldError]

    try {
      val pxml = PXML.fromPND(pndFile.is)
      name(pxml.id)
      version.fromTuple(pxml.version)
      pxml.description.foreach(d => LocalizedPackageDescription
                               .create.owner(this).string(d._1).locale(d._2).save)

      pxml.title.foreach(t => LocalizedPackageTitle
                         .create.owner(this).string(t._1).locale(t._2).save)
    } catch {
      case e => errors += new FieldError(this.pndFile, Text(e.getLocalizedMessage))
    }

    //Am I making this more complicated than it is?
    trait Validee { def vaildate: List[FieldError] }
    new Validee {
       def validate = errors.toList
    }
  }
}
