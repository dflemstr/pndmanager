package se.dflemstr.pndmanager.util.entrysystem

import _root_.net.liftweb.mapper._
import _root_.scala.xml._

/** Marks anything that can represent an entry in a Mapper */
sealed trait Entry[T <: Mapper[T]] //sealed because all the subclasses we need are in this file

/** Marks an entry that can be edited by the user (read/write) */
trait Editable[T <: Mapper[T]] extends BaseOwnedMappedField[T] with Entry[T] with Visible[T]

/** Marks an entry that appears in various appearances (read-only) */
trait Visible[T <: Mapper[T]] extends Entry[T] {
  val id: String
  def asHtml: NodeSeq
  def displayHtml: NodeSeq
  def isVisibleIn(app: Appearance.Value): Boolean
}

trait APIExposed[T <: Mapper[T]] extends Visible[T] {
  def asXML: Elem
}

/** Marks an entry that's sortable. We need database access for this. */
trait Sortable[ValueType, OwnerType <: Mapper[OwnerType]]
  extends MappedField[ValueType, OwnerType] with Entry[OwnerType] with Visible[OwnerType]

/** Defines the apperances that entries can be in */
object Appearance extends Enumeration {
  val Digest, Summary, RichSummary, Detail = Value
}

/** Display the entry in digests, summaries/lists and detail views */
trait ShowInDigest[T <: Mapper[T]] extends Visible[T] {
  def isVisibleIn(app: Appearance.Value) = app match {
    case _ => true //This thing appears in all apearances
  }
}

/** Display the entry in summaries/lists and detail views */
trait ShowInSummary[T <: Mapper[T]] extends Visible[T] {
  def isVisibleIn(app: Appearance.Value) = app match {
    case Appearance.Summary | Appearance.RichSummary | Appearance.Detail => true
    case _ => false
  }
}

/** Display the entry in summaries/lists and detail views */
trait ShowInRichSummary[T <: Mapper[T]] extends Visible[T] {
  def isVisibleIn(app: Appearance.Value) = app match {
    case Appearance.RichSummary | Appearance.Detail => true
    case _ => false
  }
}

/** Display the entry only in detail views */
trait ShowInDetail[T <: Mapper[T]] extends Visible[T] {
  def isVisibleIn(app: Appearance.Value) = app match {
    case Appearance.Detail => true
    case _ => false
  }
}

/** Use a custom method to determine if the field is visible */
trait ShowCustom[T <: Mapper[T]] extends Visible[T] {
  def isVisibleIn(app: Appearance.Value) = cond
  def cond: Boolean
}
