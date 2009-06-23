package se.dflemstr.pndmanager.comet

import util._
import net.liftweb._
import http._
import js.JsCmds.Noop
import util._
import Helpers._
import widgets.flot._

class SystemMonitor extends CometActor {
  override def defaultPrefix = Full("graph")

  var options : FlotOptions = new FlotOptions {}
  var series : List [FlotSerie] = Nil

  val idPlaceholder = "monitor_graph"
  
  def render = bind("monitor",
                    "graph" -> Flot.render(idPlaceholder, series, options, Noop))

  override def localSetup {
    DataAccumulator !? AddListener(this) match {
      case InitialData(FlotInfo(_, s, o)) =>
        options = o
        series = s
    }
    super.localSetup
  }
  
  override def localShutdown {
    DataAccumulator ! RemoveListener(this)
    super.localShutdown
  }

  override def lowPriority : PartialFunction[Any, Unit] = {
    case NewData(d) =>
      partialUpdate(JsFlotAppendData(idPlaceholder, d.series, d.datas, true))
  }
}
