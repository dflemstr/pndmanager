package se.dflemstr.pndmanager.snippet

import _root_.se.dflemstr.pndmanager.model.{Package, Category}
import _root_.se.dflemstr.pndmanager.util._
import _root_.scala.xml.{NodeSeq,Text}
import _root_.net.liftweb.http._
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.widgets.flot._

/** Provides various package management snippets */
class Packages {
  /** A snippet that inserts the package count where it's used */
  def count: NodeSeq = Text(Package.count.toString)

  /** Create a small digest with the most important package information */
  def digest(template: NodeSeq): NodeSeq = {
    Package.findAll(StartAt(0), MaxRows(10), OrderBy(Package.updatedOn, Ascending))
      .flatMap(x => bind("digest", template,
                         "name" -> x.name.asHtml,
                         "title" -> x.title.asHtml,
                         "updated" -> x.updatedOn.asHtml,
                         "version" -> x.version.asHtml,
                         "pnd" -> x.pndFile.asHtml))
  }

  def categoryDiagram(template: NodeSeq): NodeSeq = {
    val id = Helpers.randomInt(1000)

    def makeDiagram: NodeSeq = {
      val counts: Map[String, Int] = Map(Category.findAll
        .map(c => c.name.is -> Package.findAll(By(Package.category, c)).length):_*)

      import scala.util.Sorting._

      val keys = counts.keys.toList.toArray
      quickSort(keys)

      var count = 0
      val series = keys.map(e => {
        val x = counts(e)
        count += 1
        new FlotSerie {
          override val data = List((count.toDouble, x.toDouble))
          override val label = Full(e)
          override val bars = Full(new FlotBarsOptions {
            override val show = Full(true)
          })
        }
      }).toList

      val opts = new FlotOptions {
        override val legend = Full(new FlotLegendOptions {
          override val show = Full(true)
          override val noColumns = Full(2)
        })

        override val yaxis = Full(new FlotAxisOptions {
          override val min = Full(0.0)
          //TODO: override val minTickSize = Full(1.0)
          override val tickDecimals = Full(0.0)
        })

        override val xaxis = Full(new FlotAxisOptions {
          override val ticks = List(0.0)
        })
      }

      val t = <div id={"diagram" + id} style="height: 300px; width: 400px;" class="diagram-graph"></div>

      CustomFlot.render("diagram" + id, series, opts, CustomFlot.script(t)) ++ t
    }

    bind("diagram", template,
         "graph" -> makeDiagram)
  }
}
