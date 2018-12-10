package com.github.grender.wv_tt

import com.github.grender.wv_tt.Exchange.{ClientAssets, FullState}
import com.typesafe.scalalogging.StrictLogging

import scala.io.Source

object Main extends App with StrictLogging {

  import cats.syntax.all._

  val orderStorage = FileUtils.loadOrders(Source.fromFile("orders.txt"))
  val clients      = FileUtils.loadClients(Source.fromFile("clients.txt"))
  val buyOrders    = orderStorage.buyOrders
  val sellOrders   = orderStorage.sellOrders
  val startState = ProcessingStepState(
    clientAssets = clients,
    sellOrders = sellOrders
  )

  println(s"""
      |Buy orders count : ${buyOrders.length}
      |Sell orders count: ${sellOrders.length}
      |
      |Starting process...""".stripMargin)

  val statesChain = buyOrders
    .foldLeft(ClientAssets(Seq()).pure[FullState]) {
      case (state, buyOrder) =>
        state.flatMap(_ => Exchange.updateState(buyOrder))
    }

  val (state, result) = statesChain.run(startState).value

  println(s"""
      |Finish!
      |
      |Successful trades: ${state.trades.length}
      |Buy order without sell order: ${state.buyOrderWithoutSeller.length}
    """.stripMargin)
  FileUtils.writeClientsResult("result.txt", result)
  println("Written to result.txt")
}
