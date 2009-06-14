package se.dflemstr.pndmanager.snippet

import scala.xml._
import net.liftweb.widgets.tablesorter.TableSorter

/** Various table-related snippets */
class Table {
  /** Use this snippet to make a table with the provided id sortable */
  def sort(xhtml: NodeSeq): NodeSeq = TableSorter(xhtml.text)
}
