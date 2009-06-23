package se.dflemstr.pndmanager.util

import scala.actors._
import net.liftweb.util._
import net.liftweb.widgets.flot._
import comet._
import java.util.Date

object SystemSensor extends java.lang.Runnable {

  @volatile private var initialized = false
  def start() = synchronized {
    if(!initialized) {
      initialized = true
      DataAccumulator.start()
      new Thread(this).start()
    }
  }

  override def run() : Unit = {
    while(true)
    {
      val time = new Date().getTime

      val runtime = Runtime.getRuntime
      val newMem = (runtime.totalMemory - runtime.freeMemory).toDouble
      val newAlloc = runtime.totalMemory.toDouble

      DataAccumulator ! Update(time, List(newMem, newAlloc))

      Thread.sleep(1000)
    }
  }
}

object DataAccumulator extends Actor {
  val MaxData = 100;

  val options = new FlotOptions {
    override val xaxis = Full(new FlotAxisOptions() {
      override val mode = Full("time")
    })
    override val yaxis = Full(new FlotAxisOptions() {
      override val min = Full(0.0)
    })
  }

  private var series: List[FlotSerie] = new FlotSerie {
    override val label = Full("Memory usage + cache")
    override val data = Nil

    override val lines = Full (new FlotLinesOptions() {
      override val show = Full(true)
    })
  } ::
  new FlotSerie {
    override val label = Full("Allocated memory")
    override val data = Nil

    override val lines = Full (new FlotLinesOptions() {
      override val show = Full(true)
    })
  } :: Nil
  
  private var listeners = List[Actor]()

  def act() = Actor.loop {
    Actor.react {
      case AddListener(l) =>
        listeners ::= l
        reply(InitialData(FlotInfo("", series, options)))
      case RemoveListener(l) =>
        listeners -= l
      case Update(t, data) =>
        val time = t.toDouble
        
        val newSeries = (series zip data).map(d =>
          new FlotSerie() {
            override val label = d._1.label
            override val data = d._1.data.takeRight(MaxData - 1) ::: List((time, d._2)) ::: Nil
          })

        series = newSeries.toList

        val newData = data.map((time, _))

        listeners.foreach(_ ! NewData(FlotNewData(series, newData)))
    }
  }
}

case class AddListener(actor: Actor)
case class RemoveListener(actor: Actor)
case class InitialData(data: FlotInfo)
case class NewData(data: FlotNewData)

case class Update(time: Long, data: List[Double])