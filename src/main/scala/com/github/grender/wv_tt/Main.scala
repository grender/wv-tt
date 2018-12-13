package com.github.grender.wv_tt

import com.github.grender.wv_tt.Exchange.ClientAssets
import com.github.grender.wv_tt.exchanges._
import com.typesafe.scalalogging.StrictLogging

import scala.collection.mutable
import scala.io.Source

object Main extends App with StrictLogging {

  import cats.syntax.all._

  def runTime[T](timerName: String, method: () => T): (Double, T) = {
    val start   = System.nanoTime()
    val result  = method()
    val finish  = System.nanoTime()
    val runtime = (finish - start) / 1e+9
    (runtime, result)
  }

  val timeResult = mutable.Map[String, List[Double]]()
  def runOnExchange[T[_]](
      exchange: AbstractExchange[T],
      startStateList: Exchange.ProcessingStepState[List]) = {

    val name = exchange.getClass.getSimpleName

    println(s"Starting $name")
    val statesChain = buyOrders
      .foldLeft(ClientAssets(Seq()).pure[exchange.FullState]) {
        case (state, buyOrder) =>
          state.flatMap(_ => exchange.updateState(buyOrder))
      }

    val startState = exchange.createProcessingStepState(startStateList)

    val (runTimeMs, runResult) =
      runTime(name, () => statesChain.run(startState).value)

    timeResult.contains(name) match {
      case false => timeResult(name) = List(runTimeMs)
      case true  => timeResult(name) = runTimeMs :: timeResult(name)
    }

    val (state, result) = runResult

    println(s"""Finish!
      |Successful trades: ${state.trades.length}
               |Buy order without sell order: ${state.buyOrderWithoutSeller.length}
    """.stripMargin)
    (state, result)
  }

  val orderStorage = FileUtils.loadOrders(Source.fromFile("orders.txt"))
  val clients      = FileUtils.loadClients(Source.fromFile("clients.txt"))
  val buyOrders    = orderStorage.buyOrders
  val sellOrders   = orderStorage.sellOrders

  val startState = new Exchange.ProcessingStepState[List](
    clientAssets = clients,
    sellOrders = sellOrders
  )
  println(s"""Buy orders count : ${buyOrders.length}
             |Sell orders count: ${sellOrders.length}
             |""".stripMargin)

  for (i <- 1 until 10) {
    runOnExchange(new ListRecursiveExchange, startState)
    runOnExchange(new ListSpanExchange, startState)
    runOnExchange(new VectorRecursiveExchange, startState)
    runOnExchange(new ArrayBufferIterateExchange, startState)
    runOnExchange(new BufferIterateExchange, startState)

  }

  case class TimeResult(list:List[Double]) {
    val min = list.min
    val avg = list.sum / list.size
    val median = {
      val sorted = list.sorted
      if(sorted.size % 2 == 1) sorted(sorted.size / 2)
      else {
        val (up, down) = sorted.splitAt(sorted.size / 2)
        (up.last + down.head) / 2
      }
    }
    val max = list.max

    override def toString = s"min=$min avg=$avg median=$median max=$max"
  }
  timeResult.toList
    .map({
      case (name, times) => name -> TimeResult(times)
    })
    .sortBy(_._2.avg)
    .foreach({
      case (name, time) => println(s"$name : $time")
    })

  //FileUtils.writeClientsResult("result.txt", result)
  //println("Written to result.txt")
}
