package bootstrap.liftweb

import _root_.se.dflemstr.pndmanager.util._
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.openid.SimpleOpenIdVendor
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.js.{JsCmds,JE}
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, ConnectionIdentifier}
import _root_.java.sql.{Connection, DriverManager}
import _root_.se.dflemstr.pndmanager.model._
import _root_.se.dflemstr.pndmanager.dispatch.FileDispatcher
import _root_.javax.servlet.http.HttpServletRequest

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    LogBoot._log4JSetup

    // The location of our webapp classes
    LiftRules.addToPackages("se.dflemstr.pndmanager")

    setHooks()

    initDB()
    schemifyMappers()
    createData()
    
    localize()
    
    buildSiteMap()
    initAjax()
    initFrameworks()
  }
  
  private def initDB() = {
    if (!DB.jndiJdbcConnAvailable_?)
      DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)

    S.addAround(DB.buildLoanWrapper)
  }

  private def schemifyMappers() =
    //Actually create tables for our model objects
    Schemifier.schemify(true, Log.infoF _, User, Package, Category,
                        LocalizedPackageDescription, LocalizedPackageTitle)

  private def createData() = {
    //This is a quick way to get categories when first starting the application
    if(Category.count == 0) {
      Props.get("pndmanager.defaultcategories", "").split(":").filter(_ matches """[\w/._\- ]+""").foreach(Category.create.name(_).save)
      Log.info("Created some categories, count: " + Category.count) //don't translate
    }
  }

  private def localize() = {
    //Load localization data
    LiftRules.resourceNames = "translations/core" ::
                              "translations/packagesystem"::
                              "translations/entrysystem" ::
                              "translations/template" ::
                              LiftRules.resourceNames

    //Adjust the page locale to fit the User locale + time zone
    LiftRules.localeCalculator = r => User.currentUser.map(_.locale.isAsLocale) openOr LiftRules.defaultLocaleCalculator(r)
    LiftRules.timeZoneCalculator = r => User.currentUser.map(_.timezone.isAsTimeZone) openOr LiftRules.defaultTimeZoneCalculator(r)
  }
  
  private def buildSiteMap() = {
    //We use defs here, because we need to reload translations every time.
    def homeText = S.?("menu.home")
    def apiinfoText = S.?("menu.apiinfo")
    def userText = S.?("menu.user")
    def packageText =  S.?("menu.package")
    
    val userMenu = Menu(Loc("user", List("user", "index"), userText), User.sitemap: _*)

    val packageMenu = Menu(Loc("package", List("packages", "index"), packageText),
                           Package.listMenu("Package.list").open_!,
                           Package.createMenu("Package.create").open_!,
                           Package.deleteMenu("Package.delete").open_!,
                           Package.viewMenu("Package.view").open_!)

    val categoryAdminMenu = Menu(Loc("category", List("categories", "index"), "[admin] Category administration", adminAuthorization),
                                 Category.menus: _*)
    val userAdminMenu = Menu(Loc("useradmin", List("useradmin", "index"), "[admin] User administration", adminAuthorization),
                             User.adminListMenu.open_!)

    //Put all of the menus in sequence
    val entries = Menu(Loc("home", List("index"), homeText)) ::
                       packageMenu :: userMenu :: categoryAdminMenu :: userAdminMenu ::
                       Menu(Loc("apiinfo", List("apiinfo"), apiinfoText)) :: Nil

    LiftRules.setSiteMap(SiteMap(entries:_*))
  }

  private def initAjax() = {
    //JS that gets executed on Ajax events
    LiftRules.ajaxStart = Full(() => JE.Call("showAjax").cmd)
    LiftRules.ajaxEnd = Full(() => JE.Call("hideAjax").cmd )
  }

  private def initFrameworks() = {
    //OpenID support
    LiftRules.dispatch.append(SimpleOpenIdVendor.dispatchPF)

    //Init Flot
    CustomFlot.init()

    PackageNotificationDispatcher.start()

    Props.get("pndmanager.repoclonepath") match {
      case Full(clonepath) =>
        new RepositoryCloner(clonepath).start()
      case _ =>
    }
  }

  def setHooks() = {
    //Convert page to UTF-8 before sending
    LiftRules.early.append(makeUtf8)

    //Make file download URLs usable
    LiftRules.dispatch.append(FileDispatcher.dispatcher)

    //Make the XML API URLs usable
    LiftRules.dispatch.append(Package.dispatch)
  }

  private def adminAuthorization = If(() => (User.currentUser.map(_.superUser.is) openOr false), () => RedirectResponse(S.referer openOr "/"))

  private def makeUtf8(req: HttpServletRequest) {
    req.setCharacterEncoding("UTF-8")
  }
}

/**
 * Database connection calculation
 */
object DBVendor extends ConnectionManager {
  private var pool: List[Connection] = Nil
  private var poolSize = 0
  private val maxPoolSize = 4

  private def createOne: Box[Connection] = try {
    val driverName: String = Props.get("db.driver") openOr
    "org.h2.Driver"

    val dbUrl: String = Props.get("db.url") openOr
    "jdbc:h2:mem:pndmanager;DB_CLOSE_DELAY=-1"

    Class.forName(driverName)

    val dm = (Props.get("db.user"), Props.get("db.password")) match {
      case (Full(user), Full(pwd)) =>
	DriverManager.getConnection(dbUrl, user, pwd)

      case _ => DriverManager.getConnection(dbUrl)
    }

    Full(dm)
  } catch {
    case e: Exception => e.printStackTrace; Empty
  }

  def newConnection(name: ConnectionIdentifier): Box[Connection] =
    synchronized {
      pool match {
	case Nil if poolSize < maxPoolSize =>
	  val ret = createOne
        poolSize = poolSize + 1
        ret.foreach(c => pool = c :: pool)
        ret

	case Nil => wait(1000L); newConnection(name)
	case x :: xs => try {
          x.setAutoCommit(false)
          Full(x)
        } catch {
          case e => try {
            pool = xs
            poolSize = poolSize - 1
            x.close
            newConnection(name)
          } catch {
            case e => newConnection(name)
          }
        }
      }
    }

  def releaseConnection(conn: Connection): Unit = synchronized {
    pool = conn :: pool
    notify
  }
}


