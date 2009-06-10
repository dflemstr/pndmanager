package se.dflemstr.pndmanager.snippet

import scala.xml._
import net.liftweb.widgets.tablesorter.TableSorter

class SortTable {
  def render(xhtml: NodeSeq): NodeSeq = TableSorter(xhtml.text)
}
