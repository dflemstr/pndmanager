package se.dflemstr.pndmanager.model

import _root_.java.util.{Date, Locale}
import _root_.java.text.DateFormat

import _root_.scala.xml._

import _root_.net.liftweb._
import java.lang.reflect.Method
import sitemap._
import Loc._
import mapper._
import http._
import util._
import Helpers._

import _root_.se.dflemstr.pndmanager.util._

object Package extends Package with LongKeyedMetaMapper[Package] {
  //--> for the database:
  override def fieldOrder = List(name, version, owner, updatedOn, pndFile)
  override def dbTableName = "packages"

  //--> for the display system:
  object displayItemsPerPage extends SessionVar[Int](20)

  //--> for the internals of Package:

  //A condition for menus that requires a user to be logged in;
  //otherwise it redirects the visitor to the log in page
  private def userAuthorization = If(User.loggedIn_? _, () => RedirectResponse(User.loginPageURL))

  //Check if the current user is allowed to change a package
  private def mutablePackage_?(p: Package) = try {
      User.loggedIn_? &&
        (p.owner == (User.currentUser openOr 0l) || (User.currentUser.map(_.superUser.is) openOr false))
    } catch {case _ => false}

  //A validation for MappedFields for checking if a date is in the future
  private def notInFuture(field: MappedDateTime[Package])(date: Date) =
    List(FieldError(field, Text("Fatal: We tried to create the package in the future." +
                                "Don't ask how that happened, neither we nor you want to know."))) //TODO: translate!
      .filter(_ => date.getTime >= System.currentTimeMillis)

  //Converts a date to HTML, automatically using the logged-in user's preferences
  private def dateAsHtml(date: Date) = {
    val formatter = DateFormat.getDateInstance(DateFormat.LONG, S.locale)
    formatter.setTimeZone(S.timeZone)

    try {Text(formatter.format(date))} catch { case _ => <em>Corrupted</em>} //TODO: translate!
  }

  private def makeForm(pkg: Package, submitMsg: String)(template: NodeSeq) = {
    val origin = S.referer openOr "/"
    val thisSnippet = S.currentSnippet

    def makePage(html: NodeSeq): NodeSeq = {
      def doEntries(temp: NodeSeq): NodeSeq = (for {
          entry <- allEntries
          if(entry match {
              case x: Editable => true
              case _ => false
            })

          instanceField = getActualBaseField(pkg, entry.asInstanceOf[Editable])
        } yield bind("entry", temp,
                     "name" -> instanceField.displayHtml,
                     "input" -> instanceField.toForm)).flatMap(x => x) //I like one-liners...

      def onSubmit() = pkg.doUpdateProcess() match {
        case Nil =>
          pkg.save
          if(submitMsg != "") S.notice(submitMsg)
          S.redirectTo(origin)
        case error =>
          S.error(error)
          thisSnippet.foreach(S.mapSnippet(_, makePage))
      }

      bind("form", html,
           "entry" -> doEntries _,
           "submit"-> ((x: NodeSeq) => SHtml.submit(x.text, onSubmit _)))
    }

    makePage(template)
  }

  private def findForList(start: Long, count: Int): List[Package] =
    findAll(StartAt[Package](start) :: MaxRows[Package](count) ::
            findForListParams :_*)

  private def findForListParams: List[QueryParam[Package]] =
    List(OrderBy(primaryKeyField, Ascending))

  private def locSnippets(listPath: List[String], editPath: List[String],
                          viewPath: List[String], deletePath: List[String],
                          snippetName: String) = new DispatchLocSnippets {
    def s(path: List[String]) = path.mkString("/", "/", "")
    val listPathString = s(listPath)
    val editPathString = s(editPath)
    val viewPathString = s(viewPath)
    val deletePathString = s(deletePath)

    val dispatch: PartialFunction[String, NodeSeq => NodeSeq] = {
      case `snippetName` => doList
    }

    def doList(in: NodeSeq): NodeSeq = {
      val itemsPerPage: Int = displayItemsPerPage
      val first = S.param("first").map(toLong) openOr 0L
      val list = findForList(first, itemsPerPage + 1)
      def entries(p: Package) = p.allEntries.filter(_ match {
          case x: Visible if (x.isVisibleIn(Appearance.Summary)) => true
          case _ => false
        }).map(_.asInstanceOf[Visible])

      def prev(in: NodeSeq): NodeSeq =
        if(first < itemsPerPage + 1)
          Nil
        else
          <a href={listPathString+"?first="+(0L max (first - displayItemsPerPage.toLong))}>{in}</a>

      def next(in: NodeSeq): NodeSeq =
        if (list.length < itemsPerPage + 1)
          Nil
        else
          <a href={listPathString+"?first="+(first + displayItemsPerPage.toLong)}>{in}</a>

      def doHeaderItems(in: NodeSeq): NodeSeq =
        entries(Package.this).flatMap(f => bind("headeritem", in, "name" -> f.displayHtml))

      def doRows(in: NodeSeq): NodeSeq =
        list.take(itemsPerPage).flatMap {c =>
          def doRowItems(in: NodeSeq): NodeSeq = entries(c)
            .flatMap(f => bind("entry", in, "value" -> f.asHtml))

          bind("row", in ,
            "entry" -> doRowItems _,
            "edit" -> ((label: NodeSeq) =>
              if(mutablePackage_?(c))
                <a href={editPathString+"/"+obscurePrimaryKey(c)}>{label}</a>
              else
                NodeSeq.Empty),

            "view" -> ((label: NodeSeq) => 
              <a href={viewPathString+"/"+obscurePrimaryKey(c)}>{label}</a>),

            "delete" ->((label: NodeSeq) =>
              if(mutablePackage_?(c))
                <a href={deletePathString+"/"+obscurePrimaryKey(c)}>{label}</a>
              else
                NodeSeq.Empty)
          )}

      bind("list", in, "headeritem" -> doHeaderItems _,
           "row" -> doRows _,
           "prev" -> prev _, "next" -> next _)

    }
  }

  private def obscurePrimaryKey(in: Package): String = obscurePrimaryKey(in.primaryKeyField.toString)
  private def obscurePrimaryKey(in: String): String = in

  //--> for the Site Map:
  def addMenu(path: List[String], snippetName: String): Box[Menu] =
    Full(Menu(Loc("package.add", path, "Upload new Package", userAuthorization, //TODO: translate!
                  Snippet(snippetName, makeForm(create, "The package was successfully added!") _)))) //TODO: translate!

  def editMenu(path: List[String], snippetName: String): Box[Menu] =
    Full(Menu(new Loc[Package] {
      def name = "package.edit"

      override val snippets: SnippetTest = {
        case (`snippetName`, Full(p: Package)) => makeForm(p, "Package successfully edited") _ //TODO: translate!
      }

      def defaultParams = Empty

      def params = Nil

      val text = new Loc.LinkText(calcLinkText _)

      def calcLinkText(in: Package): NodeSeq = Text("Edit Package") //TODO: translate!

      override val rewrite: LocRewrite =
        Full(NamedPF(name) {
          case RewriteRequest(pp , _, _)
            if pp.wholePath.startsWith(path) &&
            pp.wholePath.length == (path.length + 1) &&
            find(pp.wholePath.last).isDefined &&
            mutablePackage_?(find(pp.wholePath.last) openOr null)=>
              (RewriteResponse(path), find(pp.wholePath.last).open_!)
        })

      val link =
        new Loc.Link[Package](path, false) {
          override def createLink(in: Package) =
          Full(Text(path.mkString("/", "/", "") + "/" + obscurePrimaryKey(in)))
        }
    }))
    
  def viewMenu(path: List[String], snippetName: String): Box[Menu] =
    Full(Menu(new Loc[Package] {
      def name = "package.view"

      override val snippets: SnippetTest = {
        case (snippetName, Full(p: Package)) => displayPackage(p) _
      }

      def defaultParams = Empty

      def params = Nil

      val text = new Loc.LinkText(calcLinkText _)

      def calcLinkText(in: Package): NodeSeq = Text("View Package") //TODO: translate!

      override val rewrite: LocRewrite =
        Full(NamedPF(name) {
          case RewriteRequest(pp , _, _)
            if pp.wholePath.startsWith(path) &&
            pp.wholePath.length == (path.length + 1) &&
            find(pp.wholePath.last).isDefined =>
              (RewriteResponse(path), find(pp.wholePath.last).open_!)
        })

      def displayPackage(entry: Package)(in: NodeSeq): NodeSeq = {
        def doEntries(in: NodeSeq): NodeSeq =
          entry.allEntries.map(_ match {
            case v: Visible =>
              if(v.isVisibleIn(Appearance.Detail))
                bind("entry", in, "name" -> v.displayHtml, "value" -> v.asHtml)
              else
                Nil

            case _ => Nil
          }).flatMap(x => x)

        bind("list", in, "entry" -> doEntries _)
      }

      val link =
        new Loc.Link[Package](path, false) {
          override def createLink(in: Package) =
          Full(Text(path.mkString("/", "/", "") + "/" + obscurePrimaryKey(in)))
        }
    }))

  def listMenu(path: List[String], editPath: List[String],
               viewPath: List[String], deletePath: List[String],
               snippetName: String): Box[Menu] =
    Full(Menu(Loc("package.browse", path, "Browse Packages", //TODO: translate!
                  locSnippets(path, editPath, viewPath, deletePath, snippetName))))

  def deleteMenu(path: List[String], snippetName: String): Box[Menu] = Full(Menu(new Loc[Package] {
      def name = "package.delete"

      override val snippets: SnippetTest = {
        case (`snippetName`, Full(p: Package)) => deletePackage(p) _
      }

      def deletePackage(pkg: Package)(html: NodeSeq): NodeSeq = {
        val origin = S.referer openOr "/"

        def doEntries(in: NodeSeq): NodeSeq = allEntries.map(_ match {
            case v: Visible =>
              if(v.isVisibleIn(Appearance.Detail))
                bind("entry", in, "name" -> v.displayHtml, "value" -> v.asHtml)
              else
                Nil
            case _ => Nil
          }).flatMap(x => x)

        def doSubmit() = {
          pkg.delete_!
          S.notice("The package was deleted.") //TODO: translate!
          S.redirectTo(origin)
        }

        bind("form", html,
          "entry" -> doEntries _,
          "submit" -> ((text: NodeSeq) => SHtml.submit(text.text, doSubmit _)))
      }

      def defaultParams = Empty
      def params = Nil

      def text = new Loc.LinkText(calcLinkText _)
      def calcLinkText(in: Package) = Text("Delete") //TODO: translate!

      override val rewrite: LocRewrite =
        Full(NamedPF(name) {
            case RewriteRequest(pp , _, _)
              if pp.wholePath.startsWith(path) &&
              pp.wholePath.length == (path.length + 1) &&
              find(pp.wholePath.last).isDefined &&
              mutablePackage_?(find(pp.wholePath.last) openOr null) =>
                (RewriteResponse(path), find(pp.wholePath.last).open_!)
          })

      val link = new Loc.Link[Package](path, false) {
        override def createLink(in: Package) =
          Full(Text(path.mkString("/", "/", "") + "/" + obscurePrimaryKey(in)))
      }
    }))
}

class Package extends LongKeyedMapper[Package] with IdPK {
  import Package._

  //This should actually be in the Meta singleton, but I don't want to use reflection
  def allEntries: List[Entry] =
    List(title, name, version, description, owner, updatedOn, pndFile)
    
  def getSingleton = Package
    
  def downloadLoc = Loc("package-" + name.is + "-" + version.is,
                        List("package", name.is + "-" + version.toHumanReadable + ".pnd"), "Download") //TODO: translate

  object owner extends MappedLongForeignKey(this, User) with ShowInSummary {
    override def displayName = "Owner" //TODO: translate!
    override def asHtml = Text(User.findByKey(is).map(_.nickname.is) openOr "Unknown") //TODO: translate!
  }

  object updatedOn extends MappedDateTime(this) with ShowInDetail {
    override def displayName = "Updated" //TODO: translate!
    override def validations = super.validations ::: List(notInFuture(this) _)
    override def asHtml = dateAsHtml(is)
  }

  object pndFile extends MappedBinary(this) with Editable with ShowInDigest {
    override def displayName = "PND File"
    override def validations = notZeroSize _ :: containsPXML _ :: super.validations

    def notZeroSize(data: Array[Byte]) =
      List(FieldError(this, Text("The PND file didn't contain anything at all!"))) //TODO: translate!
        .filter(_ => data == null || data.length == 0)

    //the second tuple element here contains List[FieldError]
    def containsPXML(data: Array[Byte]) = try {
      PND(data).PXMLdata.open_!
      Nil
    } catch {case _ => List(FieldError(this, Text("The PND field did not contain valid PXML data!")))} //TODO: translate!

    override def _toForm = Full(SHtml.fileUpload(storeTheFile))

    def storeTheFile(file: FileParamHolder) =
      set(file.file) //store the binary information of the file
      
    override def asHtml = <a href={downloadLoc.createLink(NullLocParams)}>{downloadLoc.linkText openOr "Download"}</a> //TODO: translate!
  }

  object name extends MappedString(this, 64) with ShowInDigest {
    override def displayName = "Name" //TODO: translate!

    override def dbIndexed_? = true
  }

  object version extends MappedString(this, 32) with ShowInDigest {
    //The actual value of this is a 16 char hex string, so
    //that it becomes easy to sort versions since it can be done alphabetically
    // (And I didn't want to create a custom MappedField just for this)
    override def displayName = "Version" //TODO: translate!

    protected def pad(value: String) =
      "0" * (8 - value.length) + value

    def valueFromTuple(version: (Int, Int, Int, Int)) = {
      def toHex(i: Int) = pad(Integer.toHexString(i))
      (toHex(version._1) + toHex(version._2) + toHex(version._3) + toHex(version._4))
    }

    def fromTuple(version: (Int, Int, Int, Int)) = 
      set(valueFromTuple(version))
    
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

  object description extends ShowInDetail {
    def displayHtml = Text("Description") //TODO: translate!

    def asHtml = {
      val descriptions = LocalizedPackageDescription.findAll(
        By(LocalizedPackageDescription.owner, Package.this))
      
      val localized = descriptions.find(_.locale == S.locale)
      val en_US = descriptions.find(_.locale == Locale.US)

      (localized, en_US) match {
        case (Some(descr), _) => Text(descr.string)
        case (None, Some(descr)) => Text(descr.string)
        case (None, None) => <em>({"No translation found for %locale%" replace ("%locale%", S.locale.toString)})</em> //TODO: translate!
      }
    }
  }

  object title extends ShowInSummary {
    def displayHtml = Text("Title") //TODO: translate!

    def asHtml = { //TODO: eliminate this code duplication
      val titles = LocalizedPackageTitle.findAll(
        By(LocalizedPackageTitle.owner, Package.this))

      val localized = titles.find(_.locale == S.locale)
      val en_US = titles.find(t => t.locale == "en_US" || t.locale == "en")

      (localized, en_US) match {
        case (Some(title), _) => Text(title.string)
        case (None, Some(title)) => Text(title.string)
        case (None, None) => <em>({"No translation found for %locale%" replace ("%locale%", S.locale.toString)})</em> //TODO: translate!
      }
    }
  }

  /**
   * Reads the pndFile field and populates all the other fields and entries with
   * acquired information.
   */
  def doUpdateProcess(): List[FieldError] = pndFile.validate match {
    case Nil =>
      this.save //just so that we get an ID; neccessary for associating localized strings
      try {
        val pxml = PXML.fromPND(pndFile.is).open_! //throw an exception; we don't care here
        Log.info(pxml.tree)

        pxml.validateId() //throws exceptions with explanations
        name(pxml.id)

        pxml.validateVersion()
        version.fromTuple(pxml.version)

        //The description will be Nil automatically if none were found.
        if(pxml.description.length == 0)
          S.warning("The PND didn't contain any valid content descriptions; no descriptions will be shown!") //TODO: translate!

        pxml.description.foreach(d => LocalizedPackageDescription
                                 .create.owner(this).string(d._1).locale(d._2).save)

        //The title will be Nil automatically if none were found.
        if(pxml.title.length == 0)
          S.warning("The PND didn't contain any valid titles; no titles will be shown!") //TODO: translate!
          
        pxml.title.foreach(t => LocalizedPackageTitle
                           .create.owner(this).string(t._1).locale(t._2).save)

        owner(User.currentUser)
        updatedOn(new Date())

        Nil
      } catch {
        //If we get errors, clean up and report browser-friendly errors
        case e => {this.delete_!; List(FieldError(this.pndFile, Text(e.getMessage)))}
      }
    case error => error //if the PND itself was erroneous, then step out instantly
  }
}

/** Marks anything that can represent an entry in a Mapper */
trait Entry

/** Marks an entry that can be edited by the user (read/write) */
trait Editable extends BaseOwnedMappedField[Package] with Entry with Visible

/** Marks an entry that appears in various appearances (read-only) */
trait Visible extends Entry {
  def asHtml: NodeSeq
  def displayHtml: NodeSeq
  def isVisibleIn(app: Appearance.Value): Boolean
}

/** Defines the apperances that entries can be in */
object Appearance extends Enumeration {
  val Digest, Summary, Detail = Value
}

/** Display the entry in digests, summaries/lists and detail views */
trait ShowInDigest extends Visible {
  def isVisibleIn(app: Appearance.Value) = app match {
    case _ => true //This thing appears in all apearances
  }
}

/** Display the entry in summaries/lists and detail views */
trait ShowInSummary extends Visible {
  def isVisibleIn(app: Appearance.Value) = app match {
    case Appearance.Summary | Appearance.Detail => true
    case _ => false
  }
}

/** Display the entry only in detail views */
trait ShowInDetail extends Visible {
  def isVisibleIn(app: Appearance.Value) = app match {
    case Appearance.Detail => true
    case _ => false
  }
}

/** Use a custom method to determine if the field is visible */
trait Custom extends Visible {
  def isVisibleIn(app: Appearance.Value) = cond
  def cond: Boolean
}