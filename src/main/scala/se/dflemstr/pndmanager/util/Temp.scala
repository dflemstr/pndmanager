package se.dflemstr.pndmanager.util

import scala.io._
import scala.xml._
class Temp {
  def lol {
    val file = Source.fromFile("stuff")

    val lol: NodeSeq = (for {
      line <- file.getLines
      data = line.split("=")
      key = data(0)
      value = (data drop 1).mkString
    } yield key match {
      case "" | null => Nil
      case s if s matches """\s*""" => Nil
      case s if s contains "#" => Nil
      case _ =>
        println("key=" + key)
        println("value=" + value)
        <entry key={key}>{value}</entry>
    }).toList.flatMap(x => x)

    println(lol)
  }
}
