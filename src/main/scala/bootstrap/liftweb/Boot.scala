package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.openid.SimpleOpenIdVendor
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.js.{JsCmds,JE}
import _root_.net.liftweb.sitemap._
import Loc._
import Helpers._
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
    // The location of our webapp classes
    LiftRules.addToPackages("se.dflemstr.pndmanager")

    initDispatchers()

    initDB()
    schemifyMappers()

    localize()

    buildSiteMap()

    initAjax()

    initFrameworks()
  }

  private def initDispatchers() = {
    LiftRules.dispatch.append(FileDispatcher.dispatcher)
  }
  
  private def initDB() = {
    if (!DB.jndiJdbcConnAvailable_?)
      DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)

    S.addAround(DB.buildLoanWrapper)
  }

  private def schemifyMappers() =
    Schemifier.schemify(true, Log.infoF _, User, Package, Category,
                        LocalizedPackageDescription, LocalizedPackageTitle)

  private def localize() = {
    LiftRules.localeCalculator = r => User.currentUser.map(_.locale.isAsLocale) openOr LiftRules.defaultLocaleCalculator(r)
    LiftRules.timeZoneCalculator = r => User.currentUser.map(_.timezone.isAsTimeZone) openOr LiftRules.defaultTimeZoneCalculator(r)
  }
  
  private def buildSiteMap() = {
    val userMenu = Menu(Loc("userInfo", List("user", "index"), "User"), User.sitemap: _*)
    val packageMenu = Menu(Loc("packageInfo", List("packages", "index"), "Packages"), Package.menus: _*)
    val entries = Menu(Loc("home", List("index"), "Home")) ::
      packageMenu :: userMenu :: Nil

    LiftRules.setSiteMap(SiteMap(entries:_*))
  }

  private def initAjax() = {
    //JS that gets executed on Ajax events
    LiftRules.ajaxStart = Full(() => JE.Call("showAjax").cmd)
    LiftRules.ajaxEnd = Full(() => JE.Call("hideAjax").cmd )
  }

  private def initFrameworks() = {
    //Load localization data
    LiftRules.resourceNames = "pndmanager" :: LiftRules.resourceNames

    //OpenID support
    LiftRules.dispatch.append(SimpleOpenIdVendor.dispatchPF)

    //Init the TableSorter widget
    net.liftweb.widgets.tablesorter.TableSorter.init()
  }

  def setHooks() = {
    //Convert page to UTF-8 before sending
    LiftRules.early.append(makeUtf8)
  }

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


