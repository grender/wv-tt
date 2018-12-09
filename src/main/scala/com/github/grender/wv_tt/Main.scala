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

  val statesChain = buyOrders
    .foldLeft(ClientAssets(Seq()).pure[FullState]) {
      case (state, buyOrder) =>
        state.flatMap(_ => Exchange.updateState(buyOrder))
    }

  val result = statesChain.runA(startState).value
  FileUtils.writeClientsResult("result.txt", result)
}
