package se.dflemstr.pndmanager.util

import _root_.net.liftweb.widgets.flot._
import scala.xml.{NodeSeq, Node, PCData, Text, Unparsed}
import _root_.net.liftweb.http.{LiftRules}
import _root_.net.liftweb.http.js._
import JsCmds._
import JE._
import _root_.net.liftweb.util._
import Helpers._

object CustomFlot
{
  def init() {
    import net.liftweb.http.ResourceServer

    ResourceServer.allow({
        case "customflot" :: "jquery.flot.pack.js" :: Nil => true
        case "customflot" :: "excanvas.pack.js" :: Nil => true
      })
  }

  def script(xml: NodeSeq): JsCmd =
    (xml \ "script").map(x => JsRaw(x.text).cmd).foldLeft(Noop)(_ & _)

  def render(idPlaceholder: String,
             datas: List[FlotSerie],
             options: FlotOptions,
             script: JsCmd,
             caps: FlotCapability*
  ): NodeSeq =
  {
    val ieExcanvasPackJs = Unparsed("<!--[if IE]><script language=\"javascript\" type=\"text/javascript\" src=\"" +
                                    net.liftweb.http.S.contextPath + "/" +
                                    LiftRules.resourceServerPath + "/customflot/excanvas.pack.js\"></script><![endif]-->")

    <head>
      <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/customflot/jquery.flot.pack.js"}></script>
      {ieExcanvasPackJs}
      {
        Script(_renderJs(idPlaceholder, datas, options, script, caps :_*))
      }
      <style type="text/css">
        .legend table {{
          width: auto;
        }}
      </style>
    </head>
  }

  def renderCapability (fRender: FlotCapability => JsCmd, caps: FlotCapability *): JsCmd =
  caps.foldLeft(Noop)((js, cap) => js & fRender(cap))

  def renderJs (
    idPlaceholder : String,
      datas : List [FlotSerie],
      options : FlotOptions,
      script: JsCmd,
      caps : FlotCapability *): JsCmd =
  datas match {
    case Nil => renderFlotHide(idPlaceholder, caps: _*)

    case _ => renderVars(idPlaceholder, datas, options) &
      renderFlotShow(idPlaceholder, datas, options, script, caps :_*)
  }

  def renderFlotHide (idPlaceholder: String, caps: FlotCapability *): JsCmd =
  JsHideId(idPlaceholder) &
  renderCapability (c => c.renderHide(), caps :_*)

  def renderFlotShow (
    idPlaceholder: String,
      datas: List [FlotSerie],
      options: FlotOptions,
      script: JsCmd,
      caps: FlotCapability *): JsCmd = {

    val main = FlotInfo (idPlaceholder, datas, options)

    JsShowId(idPlaceholder) &
    renderCapability (c => c.renderShow (), caps :_*) &
    JsRaw(
      "var plot_" + idPlaceholder +
      " = jQuery.plot(jQuery(" + ("#"+idPlaceholder).encJs +
      "), datas_" + idPlaceholder +
      ", options_" + idPlaceholder + ")") &
    renderCapability (c => c.render (main), caps :_*) &
    script
  }

  private def _renderJs (
    idPlaceholder : String,
      datas : List [FlotSerie],
      options : FlotOptions,
      script: JsCmd,
      caps : FlotCapability*): JsCmd = {
    renderVars (idPlaceholder, datas, options) &
    OnLoad(
      (datas match {
          case Nil => renderFlotHide(idPlaceholder, caps : _*)
          case _ => renderFlotShow(idPlaceholder, datas, options, script,
                                   caps : _*)
        }))


  }

  def renderOneValue (one: (Double, Double)) : JsExp =
  one match {
    case (Math.NaN_DOUBLE, _) => JsNull
    case (_, Math.NaN_DOUBLE) => JsNull
    case (a, b) => JsArray(a, b)
  }

  def renderValues(values: List[(Double, Double)]): JsExp =
  JsArray(values.map(renderOneValue) :_*)

  def renderDataSerie(idPlaceholder: String)(data: (FlotSerie, Int)): JsCmd =
  JsCrVar("data_"+idPlaceholder+"_"+(data._2 + 1), renderValues(data._1.data))

  def renderVars (idPlaceholder : String,
                  datas: List[FlotSerie],
                  options: FlotOptions): JsCmd =
  datas match {
    case Nil => Noop

    case _ =>
      datas.zipWithIndex.map(renderDataSerie(idPlaceholder)).
      reduceLeft(_ & _) &
      JsCrVar("datas_"+idPlaceholder, renderSeries(datas, idPlaceholder)) &
      JsCrVar("options_"+idPlaceholder, options.asJsObj)
  }

  def renderOneSerie(data: FlotSerie, idPlaceholder: String, idSerie: Int): JsObj = {
    val info: List[Box[(String, JsExp)]] =
    List(data.label.map(v => ("label", v)),
         data.lines.map(v => ("lines", v.asJsObj)),
         data.points.map(v => ("points", v.asJsObj)),
         data.bars.map(v => ("bars", v.asJsObj)),
         data.color.map {
        case Left(c) => ("color", c)
        case Right(c) => ("color", c)
      },
         data.shadowSize.map(s => ("shadowSize", s)),
         Full(("data", JsVar("data_"+idPlaceholder + "_" + idSerie))))

    JsObj(info.flatten(_.toList) :_*)
  }

  def renderSeries(datas: List[FlotSerie], idPlaceholder: String): JsArray =
  JsArray(datas.zipWithIndex.map{
      case (d, idx) => renderOneSerie(d, idPlaceholder, idx + 1)
    } :_*)
}


