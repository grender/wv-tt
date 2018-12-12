package com.github.grender.wv_tt

import com.github.grender.wv_tt.Exchange.ClientAssets
import com.typesafe.scalalogging.StrictLogging

import scala.collection.mutable
import scala.io.Source

object Main extends App with StrictLogging {

  import cats.syntax.all._

  def runTime[T](timerName: String, method: () => T): (Double,T) = {
    val start  = System.nanoTime()
    val result = method()
    val finish = System.nanoTime()
    val runtime = (finish - start) / 1e+9
    (runtime,result)
  }

  val timeResult = mutable.Map[String,List[Double]]()
  def runOnExchange[T[_]](
      exchange: AbstractExchange[T],
      startStateList: Exchange.ProcessingStepState[List]) = {
    println(s"Starting ${exchange.getClass.getSimpleName}")
    val statesChain = buyOrders
      .foldLeft(ClientAssets(Seq()).pure[exchange.FullState]) {
        case (state, buyOrder) =>
          state.flatMap(_ => exchange.updateState(buyOrder))
      }

    val startState = exchange.createProcessingStepState(startStateList)
    val name = exchange.getClass.getSimpleName
    val (runTimeMs, runResult)=runTime(name,
            () => statesChain.run(startState).value)

    timeResult.contains(name) match {
      case false=> timeResult(name) = List(runTimeMs)
      case true => timeResult(name) = runTimeMs :: timeResult(name)
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

  for(i<-1 until 5) {
    runOnExchange(new ListRecursiveExchange, startState)
    runOnExchange(new ListSpanExchange, startState)
    runOnExchange(new VectorRecursiveExchange, startState)
    runOnExchange(new ArrayBufferIterateExchange, startState)
    runOnExchange(new BufferIterateExchange, startState)

  }

  timeResult.toList.map({
                       case (name,times)=> name -> times.sum/times.size
                     })
    .sortBy(_._2)
    .foreach({
               case (name,time)=> println(s"$name : $time")
             })

  //FileUtils.writeClientsResult("result.txt", result)
  //println("Written to result.txt")
}
