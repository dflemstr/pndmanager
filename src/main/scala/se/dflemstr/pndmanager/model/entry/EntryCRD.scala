package se.dflemstr.pndmanager.model.entry

import _root_.net.liftweb._
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.mapper._

import _root_.scala.xml._
import _root_.scala.collection.mutable

/** Gives a MetaMapper that provides Mappers with Entries the ability to Create, Read and Delete items easily */
trait EntryCRD[KeyType, MapperType <: EntryProvider[KeyType, MapperType]]
    extends KeyedMetaMapper[KeyType, MapperType] {
  this: MapperType =>

  /*
   * --- Here's all the extendable stuff: ---
   */

  /** The short, descriptive and URL-friendly name of one handled CRD item, used for menus etc */
  def baseName: String = "item"

  override def dbTableName = baseName + "s"

  def spamProtection = false
  def spamProtectionTimeout = 600000 // 5 minutes
  def spamTimeMessage = "Wait another %time% seconds."
  
  def createMenuName = S.?("entry.create") replace ("%basename%", baseName)
  def viewMenuName = S.?("entry.view") replace ("%basename%", baseName)
  def listMenuName = S.?("entry.list") replace("%basename%", baseName + "s")
  def deleteMenuName = S.?("entry.delete") replace ("%basename%", baseName)

  def createPath = List(baseName + "s", "create")
  def viewPath = List(baseName + "s", "view")
  def listPath = List(baseName + "s", "list")
  def deletePath = List(baseName + "s", "delete")

  def createAction = "create"
  def listAction = "list"
  def viewAction = "view"
  def deleteAction = "delete"

  def createSuccededMsg = "Created"
  def deleteSuccededMsg = "Deleted"

  def createDeniedMsg = "Can't create!"
  def deleteDeniedMsg = "Can't delete!"

  def createSubmitButton = "Create"
  def deleteSubmitButton = "Delete"

  def createBindParams: List[BindParam] = Nil
  def viewBindParams: List[BindParam] = Nil
  def listBindParams(redraw: () => JsCmd): List[BindParam] = Nil
  def deleteBindParams: List[BindParam] = Nil

  def createAuthorization: Boolean = true
  def deleteAuthorization(m: MapperType): Boolean = true

  /** Gets called on submit; is supposed to populate fields of various kinds and check for errors */
  def creationProcess(mapper: MapperType): List[FieldError]

  /** The primary key for the specified mapper, as an URL-friendly string*/
  def urlFriendlyPrimaryKey(in: MapperType): String = 
    urlFriendlyPrimaryKey(in.primaryKeyField.toString)

  /**
   * The primary key as an URL-friendly string, created from
   * the text representation of a mapper's primary key by default
   */
  def urlFriendlyPrimaryKey(in: String): String = in

  /** The paramters used when creating lists of KeyType */
  def listQueryParams: List[QueryParam[MapperType]] =
    (
      for {
        toSort <- Sortings
        if(toSort._2 != None)
      } yield OrderBy(toSort._1, toSort._2 match {
        case Some(n) => n
        case _ => null
      })
    ).toList

  /** The alternatives for "show items per page" settings */
  val displayPerPageAlternatives = List(2, 5, 10, 20, 50)

  /*
   * --- End of extendable stuff ---
   */
  
  private lazy val itemCountAlternatives = displayPerPageAlternatives.map(x => (x.toString, x.toString))

  /** The number of items displayed per page */
  object DisplayItemsPerPage extends SessionVar[Int](5)
  
  /** The index of the first item to display */
  object FirstItemIndex extends SessionVar[Int](0)

  /** Decides whether we should show RichSummary entries or not */
  object RichDisplay extends SessionVar[Boolean](false)

  object LastCreate extends SessionVar[Long](0l)

  /** Contains all the "sortings" (sort directions) for sortable entries */
  object Sortings extends SessionVar[mutable.Map[Sortable[_, MapperType], Option[AscOrDesc]]] (mutable.Map(
    entries
      .filter(_.isInstanceOf[Sortable[_, _]])
      .map(entry => (entry.asInstanceOf[Sortable[_, MapperType]], None)): _*
  ))



  /** Finds the specified number of mappers with the specified offset using the listQueryParams */
  private def findForList(start: Long, count: Int): List[MapperType] =
    findAll(StartAt[MapperType](start) :: MaxRows[MapperType](count) :: listQueryParams :_*)

  /** Check if a path provides a mapper id at its end */
  private def checkValidity(path: ParsePath, subPath: List[String]) =
    path.wholePath.startsWith(subPath) && path.wholePath.length == (subPath.length + 1) &&
      find(path.wholePath.last).isDefined
      
  /**
   * Create classes that can identify a field by count.
   * They are returned in the form of "name name-count odd/even"
   */
  private def countClass(name: String, count: Int) =
    Text(name + " " + name + "-" + count + " " + (if(count % 2 == 0) "even" else "odd"))

  /** Automatically binds a 'class'-attribute to count classes */
  private def countClassBinder(name: String, count: Int) =
    FuncAttrBindParam("class", _ => countClass(name, count), "class")

  private def getVisibleEntries(m: MapperType, appearance: Appearance.Value): List[Visible[MapperType]] =
    m.entries.map(x => {
      x match {
        case v: Visible[_] if(v.isVisibleIn(appearance)) => List(v.asInstanceOf[Visible[MapperType]])
        case _ => Nil
      }
    }).flatMap(x => x)

  private def doEntries(e: List[Entry[MapperType]], appearance: Appearance.Value)(in: NodeSeq): NodeSeq = {
    var entryCount = 0
    e.map(x => {
      x match {
        case v: Visible[_] =>
          if(v.isVisibleIn(appearance)) {
            entryCount += 1
            bind("entry", in,
                 "name" -> v.displayHtml,
                 "value" -> v.asHtml,
                 countClassBinder("entry", entryCount))
          }
          else
            Nil
        case _ => Nil
      }
    }).flatMap(x => x)
  }

  private def createRewrite(name: String, path: List[String]):
    Box[PartialFunction[RewriteRequest, (RewriteResponse, MapperType)]] =
  Full(NamedPF(name) {
    case RewriteRequest(pp @ ParsePath(_, _, true, false), _, _) if checkValidity(pp, path) =>
      (RewriteResponse(path), find(pp.wholePath.last).open_!)
  })

  private def origin = S.referer openOr "/"

  /** Creates a form for editing a MapperType */
  private def createMapper(template: NodeSeq) = {
    val now = System.currentTimeMillis
    val nextUpload = LastCreate.is + spamProtectionTimeout
    if(spamProtection && now < nextUpload) {
      <p>{spamTimeMessage replace ("%time%", ((nextUpload - now) / 1000).toString)}</p>
    } else {
      val thisSnippet = S.currentSnippet
      val mapper = EntryCRD.this.create

      /** Closure for creating the form */
      def makePage(html: NodeSeq): NodeSeq = {
        val backdoor = origin

        /** Make a form list of all the entries */
        def doEntries(temp: NodeSeq): NodeSeq = (
          for {
            entry <- entries
            if(entry match {
                case x: Editable[_] => true
                case _ => false
              })

            instanceField = getActualBaseField(mapper, entry.asInstanceOf[Editable[MapperType]])
          } yield bind("entry", temp,
                       "name" -> instanceField.displayHtml,
                       "input" -> instanceField.toForm)
        ).flatMap(x => x) //I like one-liners...

        /** The submit callback */
        def onSubmit() = creationProcess(mapper) match {
          case Nil =>
            mapper.save
            S.notice(createSuccededMsg)
            LastCreate(now)
            S.redirectTo(backdoor)
          case error =>
            S.error(error)
            thisSnippet.foreach(S.mapSnippet(_, makePage))
        }

        bind(createAction, html,
             "entry" -> doEntries _ ::
             "submit"-> ((_: NodeSeq) => SHtml.submit(createSubmitButton, onSubmit _)) ::
             createBindParams: _*)
      }

      makePage(template)
    }
  }

  def createMenu(snippetName: String): Box[Menu] = createMenu(createPath, snippetName)

  def createMenu(path: List[String], snippetName: String): Box[Menu] =
    Full(Menu(Loc(baseName + "." + createAction, path, createMenuName, 
                  If(createAuthorization _, () => RedirectResponse(origin)),
                  Snippet(snippetName, createMapper _))))

  def listSnippet(viewPath: List[String],
                  deletePath: List[String],
                  snippetName: String) = new DispatchLocSnippets {
    def dispatch = {
      case `snippetName` => listMappers _
    }

    /** Creates a relative link out of a path */
    def s(path: List[String]) = path.mkString("/", "/", "")
    
    val viewPathString = s(viewPath)
    val deletePathString = s(deletePath)
    
    /** Create a list of mappers */
    private def listMappers(template: NodeSeq): NodeSeq = {

      /**
       * The container that contains the whole list. Used to "redraw" the list with AJAX;
       * we need to be able to find the list with JS once it has been created to modify it
       */
      val containerId = S.attr(listAction + "_container_id").open_!
      //Yes, throw an exception if this fails! This attribute is really important!

      //Use AJAX to rerender the whole list on request
      def redraw() = JsCmds.SetHtml(containerId, inner)

      /** Creates a "Previous Page" link */
      def prev(itemsPerPage: Int)(in: NodeSeq): NodeSeq =
        if(FirstItemIndex < itemsPerPage)
          Nil
        else
          SHtml.a(() => {FirstItemIndex(0 max (FirstItemIndex.toInt - DisplayItemsPerPage.toInt)); redraw()}, in)

      /** Creates a "Next Page" link */
      def next(itemsPerPage: Int, length: Int)(in: NodeSeq): NodeSeq =
        if(length < itemsPerPage + 1)
          Nil
        else
          SHtml.a(() => {FirstItemIndex(FirstItemIndex.toInt + DisplayItemsPerPage.toInt); redraw()}, in)

      /** Creates headers for the list */
      def doHeaders(appearance: Appearance.Value)(in: NodeSeq): NodeSeq = {
        var headerCount = 0
        getVisibleEntries(EntryCRD.this, appearance).flatMap(f => {
          headerCount += 1
          bind("header", in,
            //The header label
            "name" -> (f match {
              case s: Sortable[_, _] => SHtml.a(() => {
                val x = s.asInstanceOf[Sortable[_ ,MapperType]]
                Sortings(x) match {
                  case None => Sortings(x) = Some(Descending)
                  case Some(Descending) => Sortings(x) = Some(Ascending)
                  case Some(Ascending) => Sortings(x) = None
                  case s => Log.info("A list sorting problem has occured! Involved value: " + s) //don't translate
                }; redraw()}, s.displayHtml)
              case _ => f.displayHtml
            }),
            //The header class
            FuncAttrBindParam("class", _ => Text((f match {
              case s: Sortable[_, _] =>
                val x = s.asInstanceOf[Sortable[_, MapperType]]
                Sortings(x) match {
                  case Some(Ascending) => "sortable ascending "
                  case Some(Descending) => "sortable descending "
                  case None => "sortable unsorted "
                }
              case _ => ""
            }) + countClass("header", headerCount)), "class")
          )
        })
      }

      /** Creates all of the rows for the list */
      def doRows(items: List[MapperType], itemsPerPage: Int, appearance: Appearance.Value)(in: NodeSeq): NodeSeq = {
        var rowCount = 0
        items.take(itemsPerPage).flatMap {c =>
          rowCount += 1
          bind("row", in ,
               "entry" -> doEntries(c.entries, appearance) _,
               "view" -> ((label: NodeSeq) =>
                 <a href={viewPathString + "/" + urlFriendlyPrimaryKey(c)}>{label}</a>),

               "delete" ->((label: NodeSeq) =>
                 if(deleteAuthorization(c))
                   <a href={deletePathString + "/" + urlFriendlyPrimaryKey(c)}>{label}</a>
                 else
                   NodeSeq.Empty),
               FuncAttrBindParam("class", _ => Text("row " + countClass("row", rowCount)), "class"))
        }
      }

      /** Closure that actually creates the list */
      def inner: NodeSeq = {
        val itemsPerPage: Int = DisplayItemsPerPage
        val firstIndex: Int = FirstItemIndex

        //The list of items to display
        val list = findForList(FirstItemIndex.toInt, itemsPerPage + 1)

        //The rich version of the list can have a completely different design
        //compared to the ordinary list, thusly:
        val richifiedTemplate =
          if(RichDisplay)
            bind(listAction, template,
                 "rich" -> ((x: NodeSeq) => x),
                 "ordinary" -> Nil)
          else
            bind(listAction, template,
                 "rich" -> Nil,
                 "ordinary" -> ((x: NodeSeq) => x))


        val appearance = if(RichDisplay) Appearance.RichSummary else  Appearance.Summary

        //Bind all the fields we need
        bind(listAction, richifiedTemplate,
             "richview" -> SHtml.ajaxCheckbox(RichDisplay, v => {RichDisplay(v); redraw()}) ::
             "itemsperpage" -> SHtml.ajaxSelect(itemCountAlternatives,
                                                Full(itemsPerPage.toString),
                                                v => {DisplayItemsPerPage(v.toInt); FirstItemIndex(0); redraw()})::
             "pagenr" -> Text((Math.ceil(firstIndex.toFloat / itemsPerPage).toInt + 1).toString) ::
             "header" -> doHeaders(appearance) _ ::
             "row" -> doRows(list, itemsPerPage, appearance) _ ::
             "prev" -> prev(itemsPerPage) _ ::
             "next" -> next(itemsPerPage, list.length) _ ::
             listBindParams(redraw): _*)
      }

      inner
    }
  }

  def listMenu(snippetName: String): Box[Menu] = listMenu(listPath, viewPath, deletePath, snippetName)

  def listMenu(path: List[String], viewPath: List[String], deletePath: List[String],
               snippetName: String): Box[Menu] =
    Full(Menu(Loc(baseName + "." + listAction, path, listMenuName,
                  listSnippet(viewPath, deletePath, snippetName))))

  def viewMenu(snippetName: String): Box[Menu] = viewMenu(viewPath, snippetName)

  def viewMenu(path: List[String], snippetName: String): Box[Menu] =
  Full(Menu(new Loc[MapperType] {
    val action = viewAction
    val name = baseName + "." + action

    override val snippets: SnippetTest = {
      case (`snippetName`, Full(m)) => displayMapper(m.asInstanceOf[MapperType]) _
    }

    def defaultParams = Empty
    def params = Nil

    val text = new Loc.LinkText(calcLinkText _)
    def calcLinkText(in: MapperType): NodeSeq = Text(viewMenuName)

    override val rewrite: LocRewrite = createRewrite(name, path)

    def displayMapper(m: MapperType)(template: NodeSeq): NodeSeq = {
      bind(action, template,
           "entry" -> doEntries(m.entries, Appearance.Detail) _ ::
           viewBindParams: _*)
    }

    val link = new Loc.Link[MapperType](path, false) {
      override def createLink(in: MapperType) =
        Full(Text(path.mkString("/", "/", "/") + urlFriendlyPrimaryKey(in)))
    }
  }))

  def deleteMenu(snippetName: String): Box[Menu] = deleteMenu(deletePath, snippetName)

  def deleteMenu(path: List[String], snippetName: String): Box[Menu] =
  Full(Menu(new Loc[MapperType] {
    val name = baseName + "." + deleteAction

    override val snippets: SnippetTest = {
      case (`snippetName`, Full(m)) =>
        val x = m.asInstanceOf[MapperType]
        if(deleteAuthorization(x))
          deleteMapper(x)
        else
          (_ => Text(deleteDeniedMsg))
    }

    def deleteMapper(m: MapperType)(template: NodeSeq): NodeSeq = {
      val backdoor = origin
      def doSubmit() = {
        m.delete_!
        S.notice(deleteSuccededMsg)
        S.redirectTo(backdoor)
      }

      bind(deleteAction, template,
           "entry" -> doEntries(m.entries, Appearance.Detail) _ ::
           "submit" -> ((_: NodeSeq)  => SHtml.submit(deleteSubmitButton, doSubmit _)) ::
           deleteBindParams: _*)
    }

    def defaultParams = Empty
    def params = Nil

    def text = new Loc.LinkText(calcLinkText _)
    def calcLinkText(in: MapperType) = Text(deleteMenuName)

    override val rewrite: LocRewrite = createRewrite(name, path)

    val link = new Loc.Link[MapperType](path, false) {
      override def createLink(in: MapperType) =
        Full(Text(path.mkString("/", "/", "/") + urlFriendlyPrimaryKey(in)))
    }
  }))
}

trait EntryProvider[K, M <: EntryProvider[K, M]] extends KeyedMapper[K, M] {
  this: M =>
  
  /** The entries managed by the CRD system */
  def entries: List[Entry[MapperType]]
}