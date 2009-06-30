package se.dflemstr.pndmanager.util

import _root_.scala.actors._
import _root_.scala.actors.Actor._
import _root_.se.dflemstr.pndmanager.model._

sealed trait NotificationMessage

case class AddNotificationListener(listener: Actor) extends NotificationMessage
case class RemoveNotificationListener(listener: Actor) extends NotificationMessage
case class NewPackageNotification(thePackage: Package) extends NotificationMessage

//TODO: actually write Comet things for this

object PackageNotificationDispatcher extends Actor {
  private var listeners: List[Actor] = Nil

  def act = {
    loop {
      react {
        case AddNotificationListener(l) => listeners ::= l
        case RemoveNotificationListener(l) => listeners -= l
        case n: NewPackageNotification => //This gets sent from the Package object
          listeners.foreach(_ ! n) //Forward it to all the listeners
      }
    }
  }
}
