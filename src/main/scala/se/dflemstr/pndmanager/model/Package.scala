package se.dflemstr.pndmanager.model

import _root_.java.util.{Date, Locale}
import _root_.java.text.DateFormat

import _root_.scala.xml._
import _root_.scala.collection.mutable

import _root_.net.liftweb._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.http._
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.http.js._

import _root_.java.awt.image.BufferedImage
import _root_.java.awt.{AlphaComposite,Image,RenderingHints}
import _root_.javax.imageio.ImageIO
import _root_.java.io.ByteArrayOutputStream

import _root_.se.dflemstr.pndmanager.util._
import _root_.se.dflemstr.pndmanager.util.binary._
import _root_.se.dflemstr.pndmanager.util.model._
import _root_.se.dflemstr.pndmanager.util.entrysystem._

/** The MetaMapper for package objects */
object Package extends Package with LongKeyedMetaMapper[Package] 
    with EntryCRD[Long, Package] with RESTApi[Long, Package] {

  override def fieldOrder = List(name, category, version, owner, updatedOn, pndFile, valid)

  //
  // The following fields are all customizations for the EntryCRD framework
  //

  /** Single-word description of this object */
  override val baseName = "package"

  override def spamProtection = true
  override def spamProtectionTimeout = Props.get("pndmanager.spamtimeout").map(_.toInt) openOr 600000
  override def spamTimeMessage = S.?("package.create.waittime")

  override def createMenuName = S.?("package.create")
  override def viewMenuName = S.?("package.view")
  override def listMenuName = S.?("package.list")
  override def deleteMenuName = S.?("package.delete")

  override def createSuccededMsg = S.?("package.create.succeded")
  override def deleteSuccededMsg = S.?("package.delete.succeded")

  override def createDeniedMsg = S.?("package.create.denied")
  override def deleteDeniedMsg = S.?("package.delete.denied")

  override def createSubmitButton = S.?("package.create.uploadbutton")
  override def deleteSubmitButton = S.?("package.delete.deletebutton")

  override lazy val digestAccessNode = "repository"
  override lazy val elementAccessNode = "package"
  override def createTag(in: NodeSeq): Elem = <pndmanager-api>{in}</pndmanager-api>
  override def createItem(in: NodeSeq, detailsLink: Boolean, item: Package): Elem = <package>{
    in ++ (
      if(detailsLink)
        <details>{S.hostAndPath + "/api/" + elementAccessNode + "/" + urlFriendlyPrimaryKey(item) + ".xml"}</details>
      else
        Nil
    )
  }</package>

  /** Contains the "search string" provided by the user */
  object FilterString extends SessionVar[String]("")

  /** Contains the category that is being filtered */
  object FilterCategory extends SessionVar[Option[Category]](None)

  /** All of the available alternatives for the category filter */
  def filterCategoryAlternatives = 
    ("0", S.?("package.categorylist.nofilter")) ::
    Category.findAll.map(x => (x.id.toString, x.name.toString)).toList

  /** The authorization needed to create packages; very simple */
  override def createAuthorization = User.loggedIn_?

  /** Tells us if we are allowed to delete the specified package */
  override def deleteAuthorization(p: Package) = try {
    User.loggedIn_? &&
      (p.owner == (User.currentUser openOr 0l) || (User.currentUser.map(_.superUser.is) openOr false))
  } catch { case _ => false }

  /** Notifies the dispatcher that a package has been created - used for notifications */
  def notifyDispatcher(p: Package) = PackageNotificationDispatcher ! NewPackageNotification(p)

  /** Additional params needed for the list template */
  override def listBindParams(redraw: () => JsCmd): List[BindParam] =
    ("searchbox" -> SHtml.ajaxText(FilterString, x => {FilterString(x); FirstItemIndex(0); redraw()})) ::
    ("categoryfilter" -> SHtml.ajaxSelect(filterCategoryAlternatives, Full(FilterCategory.get match {
        case None => "0"
        case Some(c) => c.id.toString
      }),
      (x: String) => {x match {
        case "0" | "" | null =>
          FilterCategory(None)
          FirstItemIndex(0)
          redraw()
        case s =>
          FilterCategory(Some(try {Category.find(s.toInt) openOr null} catch {case _ => null}))
          FirstItemIndex(0)
          redraw()
      }})) ::
    ("clear" -> ((s: NodeSeq) => SHtml.a(() => {FilterString(""); FirstItemIndex(0); redraw()}, s))) :: Nil

  /** A validation for MappedFields for checking if a date is in the future */
  private def notInFuture(field: MappedDateTime[Package])(date: Date) =
    List(FieldError(field, Text(S.?("package.error.future"))))
      .filter(_ => date.getTime >= System.currentTimeMillis)

  /** Converts a date to HTML, automatically using the logged-in user's preferences */
  private def dateAsHtml(date: Date) = {
    val formatter = DateFormat.getDateInstance(DateFormat.LONG, S.locale)
    formatter.setTimeZone(S.timeZone)

    try {Text(formatter.format(date))} catch { case _ => <em>{S.?("package.updated.corrupted")}</em>}
  }

  /** Find the translation most suitable for the current user from the specified strings */
  private def findBestTranslation(strings: List[LocalizedString[_]]): (String, NodeSeq) = {
    val country = S.locale.getCountry
    
    val localized = strings.find(_.locale.is == S.locale)
    val badLocalized = strings.find(_.locale.is contains country)
    val american = strings.find(_.locale.is == "en_US")
    val english = strings.find(_.locale.is contains "en")

    val best = List(localized, badLocalized, american, english)
      .find(_ match {case Some(_) => true; case None => false})

    best match {
      case Some(Some(str)) => (str.locale, Text(str.string))
      case _ => ("", <em>({S.?("package.error.notranslation") replace ("%locale%", S.locale.toString)})</em>)
    }
  }

  /** Rebuild the image set for the specified package */
  private def updateImages(p: Package, image: BufferedImage) = {
    val thumb = createResizedCopy(image, (100, 75))
    val shot = createResizedCopy(image, (200, 150))

    val thumbData = new ByteArrayOutputStream
    val shotData = new ByteArrayOutputStream

    ImageIO.write(thumb, "PNG", thumbData)
    ImageIO.write(shot, "PNG", shotData)

    p.thumbnail(thumbData.toByteArray)
    p.screenshot(shotData.toByteArray)
  }

  /** The sort statement for the current list session */
  override def listQueryParams: List[QueryParam[Package]] = 
    super.listQueryParams :::
    (FilterCategory.get match {
      case None => Nil
      case Some(cat) => List(By(category, cat))
    }) :::
    FilterString.split(' ').filter(_ matches """\w*""").map(x => Like(name, "%" + x + "%")).toList

  /** Resizes an image to the specified size */
  private def createResizedCopy(originalImage: Image, newSize: (Int, Int)): BufferedImage = {
    val scaled = new BufferedImage(newSize._1, newSize._2, BufferedImage.TYPE_INT_ARGB)
    val g = scaled.createGraphics
    g.setComposite(AlphaComposite.Src)
    g.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
                       RenderingHints.VALUE_INTERPOLATION_BICUBIC)
    g.drawImage(originalImage, 0, 0, newSize._1, newSize._2, null)
    g.dispose();
    Log.info("An image was successfully resized to " + newSize) //don't translate this
    scaled
  }


  /**
   * Reads the pndFile field and populates all the other fields and entries with
   * acquired information.
   */
  def creationProcess(p: Package): List[FieldError] = p.pndFile.validate match {
    case Nil =>
      try {
        val pnd = PND(p.pndFile.is)
        val pxml = PXML.fromPND(pnd) match {
          case Full(p) => p
          case Empty => error(S.?("package.error.nopxmlfound"))
          case Failure(x, _, _) => error(S.?("package.error.invalidpxml") replace ("%error%", x))
        }

        pxml.validateId() //throws exceptions with explanations
        p.name(pxml.id)

        pxml.validateVersion() //throws exceptions with explanations
        p.version.fromTuple(pxml.version)

        Package.findAll(By(name, p.name.is), By(version, p.version.is)).drop(1)
          .foreach(_ => error(S.?("package.error.dupe")))

        pnd.PNGdata match {
          case Full(image) => updateImages(p, image)
          case Empty => S.warning(S.?("package.warning.noscreenshot"))
          case Failure(x, _, _) => S.warning(S.?("package.warning.invalidscreenshot") replace ("%error%", x))
        }

        p.owner(User.currentUser)

        p.updatedOn(new Date())
        
        p.valid(true)

        p.save //just so that we get an ID; neccessary for associating localized strings

        //The description will be Nil automatically if none were found.
        if(pxml.description.length == 0)
          S.warning(S.?("package.warning.nodescriptions"))

        pxml.description.foreach(d => LocalizedPackageDescription
                                 .create.owner(p).string(d._1).locale(d._2).save)

        //The title will be Nil automatically if none were found.
        if(pxml.title.length == 0)
          S.warning(S.?("package.warning.notitles"))

        pxml.title.foreach(t => LocalizedPackageTitle
                           .create.owner(p).string(t._1).locale(t._2).save)

        notifyDispatcher(p)

        Nil
      } catch {
        //If we get errors, clean up and report browser-friendly errors
        case e => { p.delete_!; List(FieldError(this.pndFile, Text(e.getMessage))) }
      }
    case error => error //if the PND itself was erroneous, then step out instantly
  }
}

class Package extends LongKeyedMapper[Package] with EntryProvider[Long, Package] 
    with IdPK {
  import Package._

  def entries: List[Entry[Package]] =
    List(screenshot, thumbnail, title, name, category, version, description, owner, updatedOn, pndFile)
    
  def getSingleton = Package

  def downloadLinkText = S.?("package.download")

  def downloadLoc = Loc("package-" + name.is + "-" + version.is,
                        new Loc.Link(List("package", name.is + "-" + version.toHumanReadable + ".pnd"), false),
                        new Loc.LinkText(_ => Text(downloadLinkText)))

  /** The user that created the package, and has delete rights on it */
  object owner extends MappedLongForeignKey(this, User)
      with ShowInRichSummary[Package] with Sortable[Long, Package] with APIExposed[Package] {
    val id = "owner"
    override def displayName = S.?("package.owner")
    override def asHtml = Text(User.findByKey(is).map(_.niceName) openOr S.?("package.owner.unknown"))
    def asXML = <owner>{asHtml.text}</owner>
  }

  /** The time at which the package was last updated. Almost always is the creation date */
  object updatedOn extends MappedDateTime(this) with ShowInRichSummary[Package] with Sortable[Date, Package] with APIExposed[Package] {
    val id = "updated"
    override def displayName = S.?("package.updated")
    override def validations = super.validations ::: List(notInFuture(this) _)
    override def asHtml = dateAsHtml(is)
    def asXML = <updatedon>{is.getTime / 1000}</updatedon>
  }

  /** The actual PND file that provides all the data needed for this package */
  object pndFile extends MappedBinary(this) with Editable[Package] //TODO: make this separate so as to reduce query times + mem usage
      with ShowInDigest[Package] with APIExposed[Package] {
    val id = "pnd"
    override def displayName = S.?("package.pndfile")
    override def validations = notZeroSize _ :: containsPXML _ :: super.validations

    def notZeroSize(data: Array[Byte]) =
      List(FieldError(this, Text(S.?("package.error.emptypnd"))))
        .filter(_ => data == null || data.length == 0)

    //the second tuple element here contains List[FieldError]
    def containsPXML(data: Array[Byte]) = try {
      PND(data).PXMLdata.open_!
      Nil
    } catch {case ex => List(FieldError(this,
        Text(S.?("package.error.invalidpnd") replace("%error%", ex.getMessage))))}

    override def _toForm = Full(SHtml.fileUpload(storeTheFile))

    def storeTheFile(file: FileParamHolder) =
      set(file.file) //store the binary information of the file
      
    override def asHtml = <a href={downloadLoc.createLink(NullLocParams)} class="downloadlink">{downloadLoc.linkText openOr S.?("package.download")}</a>

    def asXML = <pndfile>{downloadLoc.createLink(NullLocParams) match {
          case Some(x) => S.hostAndPath + x
          case _ => null
        }
      }</pndfile>
  }

  /** The name of the package */
  object name extends MappedPoliteString(this, 64) with ShowInDigest[Package]
      with Sortable[String, Package] with APIExposed[Package] {
    val id = "name"

    override def displayName = S.?("package.name")

    override def dbIndexed_? = true
    
    def asXML = <name>{is}</name>
  }

  /** The category of the package. Has nothing to do with the menu category provided by PXML files */
  object category extends MappedLongForeignKey(this, Category) with ShowInSummary[Package]
      with Sortable[Long, Package] with Editable[Package] with APIExposed[Package] {
    val id = "category"

    override def displayName = S.?("package.category")
    def unknown = S.?("package.category.unknown")

    override def asHtml = Category.find(is).map(_.name.asHtml) openOr Text(unknown)

    override def validSelectValues = Full(Category.findAll.map(c => (c.id, c.name.asHtml.text)))

    def asXML = <category>{Category.find(is).map(_.name.is) openOr unknown}</category>
  }

  /** The package version */
  object version extends MappedString(this, 32) with ShowInDigest[Package]
      with Sortable[String, Package] with APIExposed[Package] {
    val id = "version"
    //The actual value of this is a 16 char hex string, making it
    //easy to sort versions since it can be done alphabetically
    //(And I didn't want to create a custom MappedField just for this)
    override def displayName = S.?("package.version")

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

    def asXML = <version>{is}</version>
  }

  /** Specifies if this package is valid or not. Almost unused. */
  object valid extends MappedBoolean(this)

  /** The localized description of this package */
  object description extends ShowInDetail[Package] {
    val id = "description"
    def displayHtml = Text(S.?("package.description"))

    def asHtml = {
      val descriptions = LocalizedPackageDescription.findAll(
        By(LocalizedPackageDescription.owner, Package.this))
      findBestTranslation(descriptions)._2
    }
  }

  /** The localized title of this package */
  object title extends ShowInRichSummary[Package] {
    val id = "title"

    def displayHtml = Text(S.?("package.title"))

    def asHtml = {
      val titles = LocalizedPackageTitle.findAll(
        By(LocalizedPackageTitle.owner, Package.this))
      findBestTranslation(titles)._2
    }
  }

  /** A thumbnail image for this package */
  object thumbnail extends MappedBinary(this) with Visible[Package] {
    val id = "thumbnail"

    override def displayHtml = Text(S.?("package.thumbnail"))

    def isVisibleIn(app: Appearance.Value) = app match {
      case Appearance.RichSummary => true
      case _ => false
    }

    def isEmpty = is == null || is.length == 0
    
    override def asHtml = <div class="thumbnail" style=" width: 100px; height: 75px;" alt="thumbnail"> {
        if(isEmpty)
          <em class="nothumbnail" style="text-align: center;">(No thumbnail)</em>
        else
          <img src={S.hostAndPath + "/thumbnail/" + urlFriendlyPrimaryKey(Package.this) + ".png"}/>
      }</div>
  }

  /** A screenshot of this package's contents */
  object screenshot extends MappedBinary(this) with ShowInDetail[Package] with APIExposed[Package] {
    val id = "screenshot"

    override def displayHtml = Text(S.?("package.screenshot"))

    def isEmpty = is == null || is.length == 0

    override def asHtml = <div class="screenshot" style="width: 200px; height: 150px;"> {
      if(isEmpty)
        <em class="noscreenshot">(No screenshot)</em>
      else
        <img src={S.hostAndPath + "/screenshot/" + urlFriendlyPrimaryKey(Package.this) + ".png"} alt="screenshot"/>
    }</div>
    def asXML = <screenshot>{S.hostAndPath + "/screenshot/" + urlFriendlyPrimaryKey(Package.this) + ".png"}</screenshot>
  }
}
