package se.dflemstr.pndmanager.snippet

import model._
import _root_.scala.xml._

/** Provides various package management snippets */
class Packages {
  /** A snippet that inserts the package count where it's used */
  def count: NodeSeq = Text(Package.count.toString)

  //TODO: Add more statistics modules

  def addForm(template: NodeSeq) = Package.add(template)

  //If you add the Package.viewMenuLoc to the menu, this maps automatically:
  //def viewForm(template: NodeSeq)
  //The same is true for Package.editMenuLoc
  //def editForm(template: NodeSeq) = Package.edit(template)
  
  def list(template: NodeSeq) = Package.list(template)
  def deleteForm(template: NodeSeq) = Package.delete(template)
}
