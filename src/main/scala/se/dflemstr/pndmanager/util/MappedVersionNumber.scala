package se.dflemstr.pndmanager.util

import net.liftweb._
import scala.xml.Text
import util._
import mapper._
import java.sql.Types
import java.lang.reflect.Method
import java.util.Date

class MappedVersionNumber[T <: Mapper[T]] (val fieldOwner: T) 
    extends MappedField[(Int, Int, Int, Int), T] {
  
  def this(fieldOwner: T, value: (Int, Int, Int, Int)) = {
    this(fieldOwner)
    set(value)
  }

  def defaultValue = (0, 0, 0, 0)
  def dbFieldClass = classOf[(Int, Int, Int, Int)]

  private var data: (Int, Int, Int, Int) = defaultValue
  private var orgData: (Int, Int, Int, Int) = defaultValue

  private def st(in: (Int, Int, Int, Int)) = {
    data = in
    orgData = in
  }

  protected def i_is_! = data
  protected def i_was_! = orgData

  override def doneWithSave() = {
    orgData = data
  }
  
  override def readPermission_? = true
  override def writePermission_? = true

  protected def i_obscure_!(in: (Int, Int, Int, Int)) = defaultValue
  protected def real_i_set_!(value: (Int, Int, Int, Int)) = {
    if(value != data)
    {
      data = value
      dirty_?(true)
    }
    data
  }
  
  protected def intToBytes(value: Int): Array[Byte] =
    (for {
      i <- 0 to 3
      offset = (3 - i) * 8
    } yield ((value >>> offset) & 0xFF).toByte).toArray

  protected def getBytes(value: (Int, Int, Int, Int)) = {
    val (ma, mi, rev, build) = value
    intToBytes(ma) ++ intToBytes(mi) ++ intToBytes(rev) ++ intToBytes(build)
  }

  def setFromAny(in: Any) = in match {
    case tuple: (Int, Int, Int, Int) => set(tuple)
    case Some(tuple: (Int, Int, Int, Int)) => set(tuple)
    case Full(tuple: (Int, Int, Int, Int)) => set(tuple)
    case n :: _ => setFromString(n.toString)
    case Some(n) => setFromString(n.toString)
    case Full(n) => setFromString(n.toString)
    case None | Empty | Failure(_,_,_) | null => set(defaultValue)
    case n => setFromString(n.toString)
  }

  def stringToVersion(in: String) = {
    val parts = in.split('.').map(_.toInt)
    (parts(0), parts(1), parts(2), parts(3))
  }

  override def asHtml = {
    val v = i_is_!
    Text(v._1 + "." + v._2 + "." + v._3 + "." + v._4)
  }

  def setFromString(in: String) = set(stringToVersion(in))

  override def targetSQLType = Types.BINARY
  def jdbcFriendly(field: String): Object = real_convertToJDBCFriendly(data)

  def real_convertToJDBCFriendly(value: (Int, Int, Int, Int)): Object = value match {
    case null => null
    case s => getBytes(value)
  }

  def buildSetBooleanValue(accessor: Method, columnName: String):
    (T, Boolean, Boolean) => Unit = null

  def buildSetDateValue(accessor: Method, columnName: String):
    (T, Date) => Unit = null

  def buildSetStringValue(accessor: Method, columnName: String):
    (T, String) => Unit =
      (inst, ver) => doField(inst, accessor, {
          case f: MappedVersionNumber[T] =>
            f.st(stringToVersion(ver))
        })

  def buildSetLongValue(accessor: Method, columnName: String):
    (T, Long, Boolean) => Unit = null

  def buildSetActualValue(accessor: Method, data: AnyRef, columnName: String):
    (T, AnyRef) => Unit =
      (inst, ver) =>
        doField(inst, accessor, {
            case f: MappedVersionNumber[T] =>
              f.st(stringToVersion(ver.toString))
          })

  def fieldCreatorString(dbType: DriverType, colName: String): String = 
    colName + " " + dbType.binaryColumnType + "(16)"

  def asJsExp = throw new NullPointerException("No way") //we won't allow this BS!
}
