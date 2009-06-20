package se.dflemstr.pndmanager.model

import _root_.java.util.{Date, Locale}
import _root_.java.text.DateFormat

import _root_.scala.xml._

import _root_.net.liftweb._
import sitemap._
import Loc._
import mapper._
import http._
import util._
import Helpers._

import _root_.se.dflemstr.pndmanager.util._

object Package extends Package with LongKeyedMetaMapper[Package] {
  //for the database:
  override def fieldOrder = List(name, version, owner, createdOn, pndFile)
  override def dbTableName = "packages"

  //for the actual usage of entries:
  def allEntries: List[Entry] =
    List(title, name, version, description, owner, createdOn, pndFile)

  //A condition for menus that requires a user to be logged in;
  //otherwise it redirects the visitor to the log in page
  private def userAuthorization = If(User.loggedIn_? _, () => RedirectResponse(User.loginPageURL))

  //Check if the current user is allowed to change a package
  private def mutablePackage_?(p: Package) =
    User.loggedIn_? &&
      (p.owner == (User.currentUser openOr 0l) || (User.currentUser.map(_.superUser.is) openOr false))

  //A validation for MappedFields for checking if a date is in the future
  private def notInFuture(field: MappedDateTime[Package])(date: Date) =
    List(FieldError(field, Text(S.?("future.package"))))
      .filter(_ => date.getTime >= System.currentTimeMillis)

  //Converts a date to HTML, automatically using the logged-in user's preferences
  private def dateAsHtml(date: Date) = {
    val formatter = DateFormat.getDateInstance(DateFormat.LONG, S.locale)
    formatter.setTimeZone(S.timeZone)

    scala.xml.Text(formatter.format(date))
  }

  private def makeForm(pkg: Package, submitMsg: String)(template: NodeSeq) = {
    val origin = S.referer openOr "/"
    val thisSnippet = S.currentSnippet

    def makePage(html: NodeSeq): NodeSeq = {
      def makeFields(temp: NodeSeq): NodeSeq = (for {
          entry <- allEntries
          if(entry match {
              case x: Editable => true
              case _ => false
            })

          instanceField = getActualBaseField(pkg, entry.asInstanceOf[Editable])
        } yield bind("field", html,
                     "name" -> instanceField.displayHtml,
                     "input" -> instanceField.toForm)).flatMap(x => x) //I like one-liners...

      def onSubmit() = pkg.doCreationProcess() match {
        case Nil =>
          pkg.save
          if(submitMsg != "") S.notice(submitMsg)
          S.redirectTo(origin)
        case error =>
          S.error(error)
          thisSnippet.foreach(S.mapSnippet(_, makePage))
      }

      bind("form", html,
           "field" -> ((x: NodeSeq) => makeFields(x)),
           "submit"-> ((x: NodeSeq) => SHtml.submit(x.text, onSubmit _)))
    }

    makePage(template)
  }

  def obscurePrimaryKey(in: Package): String = obscurePrimaryKey(in.primaryKeyField.toString)
  def obscurePrimaryKey(in: String): String = in

  def add(template: NodeSeq) = makeForm(create, "Add") _ //TODO: translate!
  def addMenuLoc(path: List[String]): Box[Menu] =
    Full(Menu(Loc("package.create", path, "Upload new Package"))) //TODO: translate!

  def editMenuLoc(path: List[String], snippetName: String): Box[Menu] =
    Full(Menu(new Loc[Package] {
      def name = "package.edit"

      override val snippets: SnippetTest = {
        case (snippetName, Full(p: Package)) => makeForm(p, "Package edited") _ //TODO: translate!
      }

      def defaultParams = Empty

      def params = Nil

      val text = new Loc.LinkText(calcLinkText _)

      def calcLinkText(in: Package): NodeSeq = Text("Edit") //TODO: translate!

      override val rewrite: LocRewrite =
        Full(NamedPF(name) {
          case RewriteRequest(pp , _, _)
            if pp.wholePath.startsWith(path) &&
            pp.wholePath.length == (path.length + 1) &&
            find(pp.wholePath.last).isDefined =>
              (RewriteResponse(path), find(pp.wholePath.last).open_!)
        })

      val link =
        new Loc.Link[Package](path, false) {
          override def createLink(in: Package) =
          Full(Text(path.mkString("/", "/", "") + "/" + obscurePrimaryKey(in)))
        }
    }))
    
  def viewMenuLoc(path: List[String], snippetName: String): Box[Menu] =
    Full(Menu(new Loc[Package] {
      def name = "View Package" //TODO: translate!

      override val snippets: SnippetTest = {
        case (snippetName, Full(p: Package)) => displayPackage(p) _
      }

      def defaultParams = Empty

      def params = Nil

      val text = new Loc.LinkText(calcLinkText _)

      def calcLinkText(in: Package): NodeSeq = Text("View") //TODO: translate!

      override val rewrite: LocRewrite =
        Full(NamedPF(name) {
          case RewriteRequest(pp , _, _)
            if pp.wholePath.startsWith(path) &&
            pp.wholePath.length == (path.length + 1) &&
            find(pp.wholePath.last).isDefined =>
              (RewriteResponse(path), find(pp.wholePath.last).open_!)
        })

      def displayPackage(entry: Package)(in: NodeSeq): NodeSeq = {
        def doItems(in: NodeSeq): NodeSeq =
          allEntries.map(_ match {
            case v: Visible =>
              if(v.isVisibleIn(Appearance.Summary))
                bind("item", in, "name" -> v.displayHtml, "value" -> v.asHtml)
              else
                Nil

            case _ => Nil
          }).flatMap(x => x)

        bind("list", in, "item" -> doItems _)
      }

      val link =
        new Loc.Link[Package](path, false) {
          override def createLink(in: Package) =
          Full(Text(path.mkString("/", "/", "") + "/" + obscurePrimaryKey(in)))
        }
    }))

  def list(template: NodeSeq) = {

  }

  def delete(template: NodeSeq) = {
    
  }
}

class Package extends LongKeyedMapper[Package] with IdPK {
  import Package._
  
  def getSingleton = Package
    
  def downloadLoc = Loc("package-" + name.is + "-" + version.is,
                        List("package", name.is + "-" + version.toHumanReadable + ".pnd"), "Download") //TODO: translate

  //The package owner/maintainer; NOT the author!
  object owner extends MappedLongForeignKey(this, User) with ShowInSummary {
    override def displayName = S.?("owner")
    override def asHtml = Text(User.findByKey(is).map(_.nickname.is) openOr "Unknown") //TODO: translate!
  }

  object createdOn extends MappedDateTime(this) with ShowInDetail {
    override def displayName = S.?("created")
    override def validations = super.validations ::: List(notInFuture(this) _)
    override def asHtml = dateAsHtml(is)
  }

  object pndFile extends MappedBinary(this) with Editable with ShowInDigest {
    override def displayName = S.?("pnd.file")
    override def validations = notZeroSize _ :: containsPXML _ :: super.validations

    def notZeroSize(data: Array[Byte]) =
      List(FieldError(this, Text("The PND file didn't contain anything at all!"))) //TODO: translate!
        .filter(_ => data.length == 0)

    //the second tuple element here contains List[FieldError]
    def containsPXML(data: Array[Byte]) = try {
      PND(data).PXMLdata.open_!
      Nil
    } catch {case _ => List(FieldError(this, Text("The PND field did not contain valid PXML data!")))} //TODO: translate!

    override def _toForm = Full(SHtml.fileUpload(storeTheFile))

    def storeTheFile(file: FileParamHolder) =
      set(file.file) //store the binary information of the file
      
    override def asHtml = <a href={downloadLoc.createLink(NullLocParams)}>{downloadLoc.linkText openOr "Download"}</a>
  }

  object name extends MappedString(this, 64) with ShowInDigest {
    override def displayName = S.?("name")

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
        case (None, None) => <em>(No translation found for {S.locale})</em> //TODO: translate!
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
        case (None, None) => <em>(No translation found for {S.locale})</em> //TODO: translate!
      }
    }
  }

  /**
   * Reads the pndFile field and populates all the other fields and entries with
   * acquired information.
   */
  def doCreationProcess(): List[FieldError] = pndFile.validate match {
    case Nil =>
      this.save //just so that we get an ID; neccessary for associating localized strings
      try {
        val pxml = PXML.fromPND(pndFile.is).open_! //throw an exception; we don't care here

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

        Nil
      } catch {
        //If we get errors, clean up and report browser-friendly errors
        case e => {this.delete_!; List(FieldError(this.pndFile, Text(e.getLocalizedMessage)))}
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
case class Custom(val cond: () => Boolean) extends Visible {
  def isVisibleIn(app: Appearance.Value) = cond()
}