package se.dflemstr.pndmanager.model

import _root_.java.util.{Date, Locale}
import _root_.java.text.DateFormat

import _root_.scala.xml._
import _root_.scala.collection.mutable

import _root_.net.liftweb._
import java.lang.reflect.Method
import sitemap._
import Loc._
import mapper._
import http._
import util._
import Helpers._
import js._
import JsCmds._

import java.awt.image.BufferedImage
import java.awt.{AlphaComposite,Image,RenderingHints}
import javax.imageio.ImageIO
import java.io.ByteArrayOutputStream

import _root_.se.dflemstr.pndmanager.util._

import entry._

//TODO: This file is far to complex! Outsource code in traits!

/** The MetaMapper for package objects */
object Package extends Package with LongKeyedMetaMapper[Package] with EntryCRD[Long, Package] {
  override def fieldOrder = List(name, category, version, owner, updatedOn, pndFile)
  override def dbTableName = "packages"

  /** Contains the "search string" provided by the user */
  object FilterString extends SessionVar[String]("")

  object FilterCategory extends SessionVar[Option[Category]](None)

  def filterCategoryAlternatives = { //TODO: translate!
    ("0", "No filter") :: Category.findAll.map(x => (x.id.toString, x.name.toString)).toList
  }

  override def createAuthorization = User.loggedIn_?

  override def deleteAuthorization(p: Package) = try {
    User.loggedIn_? &&
      (p.owner == (User.currentUser openOr 0l) || (User.currentUser.map(_.superUser.is) openOr false))
  } catch { case _ => false }


  /** A validation for MappedFields for checking if a date is in the future */
  private def notInFuture(field: MappedDateTime[Package])(date: Date) =
    List(FieldError(field, Text("Fatal: We tried to create the package in the future." +
                                "Don't ask how that happened, neither we nor you want to know."))) //TODO: translate!
      .filter(_ => date.getTime >= System.currentTimeMillis)

  /** Converts a date to HTML, automatically using the logged-in user's preferences */
  private def dateAsHtml(date: Date) = {
    val formatter = DateFormat.getDateInstance(DateFormat.LONG, S.locale)
    formatter.setTimeZone(S.timeZone)

    try {Text(formatter.format(date))} catch { case _ => <em>Corrupted</em>} //TODO: translate!
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
  def creationProcess(p: Package): List[FieldError] = pndFile.validate match {
    case Nil =>
      p.save //just so that we get an ID; neccessary for associating localized strings
      try {
        val pnd = PND(p.pndFile.is)
        val pxml = PXML.fromPND(pnd) match {
          case Full(p) => p
          case Empty => error("We were unable to find a PXML file in your PND! Is it a valid PND file?") //TODO: translate!
          case Failure(x, _, _) => error("Error while locating the PXML file (is your PND valid?): %error%" replace ("%error%", x)) //TODO: translate!
        }

        pxml.validateId() //throws exceptions with explanations
        p.name(pxml.id)

        pxml.validateVersion() //throws exceptions with explanations
        p.version.fromTuple(pxml.version)

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

        pnd.PNGdata match {
          case Full(image) => updateImages(image)
          case Empty => S.warning("Your PND does not contain a screenshot; no screenshot will be shown!") //TODO: translate!
          case Failure(x, _, _) => S.warning("We tried to find a screenshot in your PND, but were " +
                                             "unable to, so none will be displayed. The error was: %error%" replace ("%error%", x)) //TODO: translate!
        }

        p.owner(User.currentUser)

        p.updatedOn(new Date())
        
        p.valid(true)

        Nil
      } catch {
        //If we get errors, clean up and report browser-friendly errors
        case e => { p.delete_!; List(FieldError(this.pndFile, Text(e.getMessage))) }
      }
    case error => error //if the PND itself was erroneous, then step out instantly
  }
}

class Package extends LongKeyedMapper[Package] with IdPK {
  import Package._

  //This should actually be in the Meta singleton, but I don't want to use reflection
  def allEntries: List[Entry] =
    List(screenshot, thumbnail, title, name, category, version, description, owner, updatedOn, pndFile)
    
  def getSingleton = Package
    
  def downloadLoc = Loc("package-" + name.is + "-" + version.is,
                        List("package", name.is + "-" + version.toHumanReadable + ".pnd"), "Download") //TODO: translate

  object owner extends MappedLongForeignKey(this, User) with ShowInRichSummary with Sortable[Long] {
    override def displayName = "Owner" //TODO: translate!
    override def asHtml = Text(User.findByKey(is).map(_.nickname.is) openOr "Unknown") //TODO: translate!
    def asXML = <owner>{asHtml.text}</owner>
  }

  object updatedOn extends MappedDateTime(this) with ShowInRichSummary with Sortable[Date] {
    override def displayName = "Updated" //TODO: translate!
    override def validations = super.validations ::: List(notInFuture(this) _)
    override def asHtml = dateAsHtml(is)
    def asXML = <updatedon>{is.getTime / 1000}</updatedon>
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
    } catch {case ex => List(FieldError(this, Text("The PND field did not contain valid PXML data! " + ex.getMessage)))} //TODO: translate!

    override def _toForm = Full(SHtml.fileUpload(storeTheFile))

    def storeTheFile(file: FileParamHolder) =
      set(file.file) //store the binary information of the file
      
    override def asHtml = <a href={downloadLoc.createLink(NullLocParams)} class="downloadlink">{downloadLoc.linkText openOr "Download"}</a> //TODO: translate!

    def asXML = <xml:group />
  }

  object name extends MappedPoliteString(this, 64) with ShowInDigest with Sortable[String] {
    override def displayName = "Name" //TODO: translate!

    override def dbIndexed_? = true
  }

  object category extends MappedLongForeignKey(this, Category) with ShowInSummary with Sortable[Long] with Editable {
    override def displayName = "Category" //TODO: translate!

    override def asHtml = Category.find(is).map(_.name.asHtml) openOr Text("Unknown") //TODO: translate!

    override def validSelectValues = Full(Category.findAll.map(c => (c.id, c.name.asHtml.text)))
  }

  object version extends MappedString(this, 32) with ShowInDigest with Sortable[String] {
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

  object valid extends MappedBoolean(this)

  object description extends ShowInDetail {
    def displayHtml = Text("Description") //TODO: translate!

    def asHtml = {
      val descriptions = LocalizedPackageDescription.findAll(
        By(LocalizedPackageDescription.owner, Package.this))
      
      val localized = descriptions.find(_.locale == S.locale)
      val en_US = descriptions.find(t => t.locale == "en_US" || t.locale == "en")

      (localized, en_US) match {
        case (Some(descr), _) => Text(descr.string)
        case (None, Some(descr)) => Text(descr.string)
        case (None, None) => <em>({"No translation found for %locale%" replace ("%locale%", S.locale.toString)})</em> //TODO: translate!
      }
    }
  }

  object title extends ShowInRichSummary {
    def displayHtml = Text("Title") //TODO: translate!

    def asHtml = {
      val titles = LocalizedPackageTitle.findAll(
        By(LocalizedPackageTitle.owner, Package.this))

      val localized = titles.find(_.locale == S.locale)
      val english = titles.find(t => t.locale == "en_US" || t.locale == "en")

      (localized, english) match {
        case (Some(title), _) => Text(title.string)
        case (None, Some(title)) => Text(title.string)
        case (None, None) => <em>({"No translation found for %locale%" replace ("%locale%", S.locale.toString)})</em> //TODO: translate!
      }
    }
  }

  object thumbnail extends MappedBinary(this) with Visible {
    override def displayHtml = Text("Preview") //TODO: translate!

    def isVisibleIn(app: Appearance.Value) = app match {
      case Appearance.RichSummary => true
      case _ => false
    }

    def isEmpty = is == null || is.length == 0
    
    override def asHtml = <div class="thumbnail" style=" width: 100px; height: 75px;" alt="thumbnail"> {
        if(isEmpty)
          <em class="nothumbnail" style="text-align: center;">(No thumbnail)</em>
        else
          <img src={"/thumbnail/" + obscurePrimaryKey(Package.this) + ".png"}/>
      }</div>
  }

  object screenshot extends MappedBinary(this) with ShowInDetail {
    override def displayHtml = Text("Screenshot") //TODO: translate!

    def isEmpty = is == null || is.length == 0

    override def asHtml = <div class="screenshot" style="width: 200px; height: 150px;"> {
        if(isEmpty)
          <em class="noscreenshot">(No screenshot)</em>
        else
          <img src={"/screenshot/" + obscurePrimaryKey(Package.this) + ".png"} alt="screenshot"/>

     }</div>
  }

  def updateImages(image: BufferedImage) = {
    val thumb = createResizedCopy(image, (100, 75))
    val shot = createResizedCopy(image, (200, 150))

    val thumbData = new ByteArrayOutputStream
    val shotData = new ByteArrayOutputStream

    ImageIO.write(thumb, "PNG", thumbData)
    ImageIO.write(shot, "PNG", shotData)

    thumbnail(thumbData.toByteArray)
    screenshot(shotData.toByteArray)
  }

}
