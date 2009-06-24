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

/** Gives a MetaMapper that provides Mappers with Entries the ability to Create, Read and Delete items easily */
trait EntryCRD[KeyType, MapperType <: EntryProvider[KeyType, MapperType]]
    extends KeyedMetaMapper[KeyType, MapperType] {
  this: MapperType =>

  /*
   * --- Here's all the extendable stuff: ---
   */

  /** The short, descriptive and URL-friendly name of one handled CRD item, used for menus etc */
  val baseName: String = "item"
  
  val createMenuName = "Create " + baseName
  val viewMenuName = "View " + baseName
  val listMenuName = "List " + baseName + "s"
  val deleteMenuName = "Delete " + baseName

  def createSuccessMessage(m: MapperType) = "The %mapper% was successfully created!" replace ("%mapper%", baseName)
  def deleteSuccessMessage = "Delete " + baseName

  val createPath = List(baseName + "s", "create")
  val viewPath = List(baseName + "s", "view")
  val listPath = List(baseName + "s", "list")
  val deletePath = List(baseName + "s", "delete")

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

  /** Finds the specified number of mappers with the specified offset using the listQueryParams */
  private def findForList(start: Long, count: Int): List[MapperType] =
    findAll(StartAt[MapperType](start) :: MaxRows[MapperType](count) :: listQueryParams :_*)

  /** Check if a path provides a mapper id at its end */
  private def checkValidity(path: ParsePath, subPath: List[String]) =
    path.wholePath.startsWith(subPath) && path.wholePath.length == (subPath.length + 1) &&
      find(path.wholePath.last).isDefined

  /** The number of items displayed per page */
  private object DisplayItemsPerPage extends SessionVar[Int](5)
  
  /** The index of the first item to display */
  private object FirstItemIndex extends SessionVar[Int](0)

  /** Decides whether we should show RichSummary entries or not */
  object RichDisplay extends SessionVar[Boolean](false)

  /** Contains all the "sortings" (sort directions) for sortable entries */
  object Sortings extends SessionVar[Map[Sortable[_, MapperType], Option[AscOrDesc]]] (Map(
      entries
        .filter(_.isInstanceOf[Sortable[_, _]])
        .map(entry => (entry.asInstanceOf[Sortable[_, MapperType]], None)): _*)
  )

  private def doEntries(m: MapperType, appearance: Appearance.Value)(in: NodeSeq): NodeSeq = m.entries.map(_ match {
    case v: Visible[_] =>
      if(v.isVisibleIn(appearance))
        bind("entry", in, "name" -> v.displayHtml, "value" -> v.asHtml)
      else
        Nil
    case _ => Nil
  }).flatMap(x => x)


  def origin = S.referer openOr "/"
  
  def createMenu(path: List[String], snippetName: String): Box[Menu] =
  Full(Menu(new Loc[NullLocParams] {
    def name = baseName + ".create"

    val text = new Loc.LinkText(calcLinkText _)
    def calcLinkText(in: MapperType): NodeSeq = Text(createMenuName)

    override val snippets: SnippetTest = {
      case (`snippetName`, _) => createMapper _
    }
    
    def params = If(createAuthorization _, () => RedirectResponse("/")) :: Nil

    /** Creates a form for editing a MapperType */
    private def createMapper(template: NodeSeq) = {
      val thisSnippet = S.currentSnippet
      val mapper = EntryCRD.this.create

      /** Closure for creating the form */
      def makePage(html: NodeSeq): NodeSeq = {
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
            S.notice(createSuccessMessage(mapper))
            S.redirectTo(origin)
          case error =>
            S.error(error)
            thisSnippet.foreach(S.mapSnippet(_, makePage))
        }

        bind("form", html,
             "entry" -> doEntries _,
             "submit"-> ((x: NodeSeq) => SHtml.submit(x.text, onSubmit _)),
             "denied" -> Nil)
      }

      makePage(template)
    }
  }))

  def listMenu(snippetName: String): Box[Menu] = listMenu(listPath, viewPath, deletePath, snippetName)

  def listMenu(path: List[String], viewPath: List[String],
               deletePath: List[String], snippetName: String): Box[Menu] =
  Full(Menu(new Loc[NullLocParams] {

    /** Creates a relative link out of a path */
    def s(path: List[String]) = path.mkString("/", "/", "")
    val listPathString = s(path)
    val viewPathString = s(viewPath)
    val deletePathString = s(deletePath)
    def name = baseName + ".list"

    override val snippets: SnippetTest = {
      case (`snippetName`, _) => listMappers _
    }

    def params = Nil

    /** Create a list of mappers */
    private def listMappers(template: NodeSeq): NodeSeq = {

      /**
       * The container that contains the whole list. Used to "redraw" the list with AJAX;
       * we need to be able to find the list with JS once it has been created to modify it
       */
      val containerId = S.attr("list_container_id").open_!
       //Yes, throw an exception if this fails! This attribute is really important!
      
      //Use AJAX to rerender the whole list on request
      def redraw() = JsCmds.SetHtml(containerId, inner)

      /** Get all the visible entries from the package */
      def entries(m: MapperType) = m.entries
        .filter(_ match {
          case x: Visible[_] if(if(RichDisplay)
                                x.isVisibleIn(Appearance.RichSummary)
                             else
                                x.isVisibleIn(Appearance.Summary)) => true
          case _ => false
        })
        .map(_.asInstanceOf[Visible[MapperType]])

      /**
       * Create classes that can identify a field by count.
       * They are returned in the form of "name-count odd/even"
       */
      def countClass(name: String, count: Int) =
        Text(name + "-" + count + " " + (if(count % 2 == 0) "even" else "odd"))

      /** Automatically binds a 'class'-attribute to count classes */
      def countClassBinder(name: String, count: Int) =
        FuncAttrBindParam("class", _ => countClass(name, count), "class")

      /** Creates a "Previous Page" link */
      def prev(itemsPerPage: Int)(in: NodeSeq): NodeSeq =
        if(FirstItemIndex < itemsPerPage)
          Nil
        else
          SHtml.a(() => {FirstItemIndex(0 max (FirstItemIndex.toInt - DisplayItemsPerPage.toInt)); redraw()}, in)

      /** Creates a "Next Page" link */
      def next(itemsPerPage: Int, length: Int)(in: NodeSeq): NodeSeq =
        if (length < itemsPerPage + 1)
          Nil
        else
          SHtml.a(() => {FirstItemIndex(FirstItemIndex.toInt + DisplayItemsPerPage.toInt); redraw()}, in)

      /** Creates headers for the list */
      def doHeaderItems(in: NodeSeq): NodeSeq = {
        var headerCount = 0
        entries(EntryCRD.this).flatMap(f => {
          headerCount += 1
          bind("header", in,
            //The header label
            "name" -> (f match {
              case s: Sortable[_, MapperType] => SHtml.a(() => {Sortings(s) match {
                  case None => Sortings(s) = Some(Descending)
                  case Some(Descending) => Sortings(s) = Some(Ascending)
                  case Some(Ascending) => Sortings(s) = None
                }; redraw()}, s.displayHtml)
              case _ => f.displayHtml
            }),
            //The header class
            FuncAttrBindParam("class", _ => Text((f match {
              case s: Sortable[_, MapperType] => Sortings(s) match {
                case Some(Ascending) => "sortable header ascending"
                case Some(Descending) => "sortable header descending"
                case None => "sortable header unsorted"
              }
              case _ => "header"
            }) + " " + countClass("header", headerCount)), "class")
          )
        })
      }

      /** Creates all of the rows for the list */
      def doRows(items: List[MapperType], itemsPerPage: Int)(in: NodeSeq): NodeSeq = {
        var rowCount = 0
        items.take(itemsPerPage).flatMap {c =>
          /** Creates all the entries in a row */
          def doRowEntries(in: NodeSeq): NodeSeq = {
            var itemCount = 0
            entries(c).flatMap(f => {
              itemCount += 1
              bind("entry", in,
                   "value" -> f.asHtml,
                   FuncAttrBindParam("class", _ => Text("entry " + countClass("entry", itemCount)), "class"))}
            )
          }

          rowCount += 1
          bind("row", in ,
            "entry" -> doRowEntries _,
            "view" -> ((label: NodeSeq) =>
              <a href={viewPathString + "/" + urlFriendlyPrimaryKey(c)}>{label}</a>),

            "delete" ->((label: NodeSeq) =>
              if(deleteAuthorization(c))
                <a href={deletePathString + "/" + urlFriendlyPrimaryKey(c)}>{label}</a>
              else
                NodeSeq.Empty),
            FuncAttrBindParam("class", _ => Text("row " + countClass("row", rowCount)), "class")
          )}
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
            bind("list", template,
                 "rich" -> ((x: NodeSeq) => x),
                 "ordinary" -> Nil)
          else
            bind("list", template,
                 "rich" -> Nil,
                 "ordinary" -> ((x: NodeSeq) => x))


        //Bind all the fields we need
        bind("list", richifiedTemplate,
             "richview" -> SHtml.ajaxCheckbox(RichDisplay, v => {RichDisplay(v); redraw()}),
             "itemsperpage" -> SHtml.ajaxSelect(itemCountAlternatives,
                                                Full(itemsPerPage.toString),
                                                v => {DisplayItemsPerPage(v.toInt); FirstItemIndex(0); redraw()}),
             "pagenr" -> Text((Math.ceil(firstIndex.toFloat / itemsPerPage).toInt + 1).toString),
             "header" -> doHeaderItems _,
             "row" -> doRows(list, itemsPerPage) _,
             "prev" -> prev(itemsPerPage) _,
             "next" -> next(itemsPerPage, list.length) _)
      }

      inner
    }
  }))

  def viewMenu(snippetName: String): Box[Menu] = viewMenu(viewPath, snippetName)

  def viewMenu(path: List[String], snippetName: String): Box[Menu] =
  Full(Menu(new Loc[MapperType] {
    def name = baseName + ".view"

    override val snippets: SnippetTest = {
      case (`snippetName`, Full(m: MapperType)) => displayMapper(m) _
    }

    def defaultParams = Empty

    def params = Nil

    val text = new Loc.LinkText(calcLinkText _)

    def calcLinkText(in: MapperType): NodeSeq = Text(viewMenuName)

    override val rewrite: LocRewrite =
      Full(NamedPF(name) {
        case RewriteRequest(pp @ ParsePath(_, _, true, false), _, _) if checkValidity(pp, path) =>
          (RewriteResponse(path), find(pp.wholePath.last).open_!)
      })

    def displayMapper(m: MapperType)(template: NodeSeq): NodeSeq = {
      bind("list", template, "entry" -> doEntries(m, Appearance.Detail) _)
    }

    val link =
      new Loc.Link[MapperType](path, false) {
        override def createLink(in: MapperType) =
          Full(Text(path.mkString("/", "/", "/") + urlFriendlyPrimaryKey(in)))
      }
  }))

  def deleteMenu(snippetName: String): Box[Menu] = deleteMenu(deletePath, snippetName)

  def deleteMenu(path: List[String], snippetName: String): Box[Menu] =
  Full(Menu(new Loc[MapperType] {
    def name = baseName + ".delete"

    override val snippets: SnippetTest = {
      case (`snippetName`, Full(m: MapperType)) =>
        if(deleteAuthorization(m))
          deleteMapper(m)
        else
          ((h: NodeSeq) => h \\ "form:denied")
    }

    def deleteMapper(m: MapperType)(template: NodeSeq): NodeSeq = {
      val origin = S.referer openOr "/"

      def doSubmit() = {
        m.delete_!
        S.notice("The %mapper% was deleted." replace ("%mapper%", baseName)) //TODO: translate!
        S.redirectTo(origin)
      }

      bind("form", template,
        "entry" -> doEntries(m, Appearance.Detail) _,
        "submit" -> ((text: NodeSeq) => SHtml.submit(text.text, doSubmit _)),
        "denied" -> Nil)
    }

    def defaultParams = Empty
    def params = Nil

    def text = new Loc.LinkText(calcLinkText _)
    def calcLinkText(in: MapperType) = Text(deleteMenuName) //TODO: translate!

    override val rewrite: LocRewrite =
      Full(NamedPF(name) {
        case RewriteRequest(pp @ ParsePath(_, _, true, false), _, _) if checkValidity(pp, path) =>
          (RewriteResponse(path), find(pp.wholePath.last).open_!)
      })

    val link = new Loc.Link[MapperType](path, false) {
      override def createLink(in: MapperType) =
        Full(Text(path.mkString("/", "/", "/") + urlFriendlyPrimaryKey(in)))
    }
  }))
}

trait EntryProvider[K, M <: EntryProvider[K, M]]
    extends KeyedMapper[K, M] {
  this: M =>
  
  /** The entries managed by the CRD system */
  def entries: List[Entry[MapperType]]
}