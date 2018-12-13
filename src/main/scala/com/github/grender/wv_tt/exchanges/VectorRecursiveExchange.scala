package com.github.grender.wv_tt.exchanges

import com.github.grender.wv_tt.{Order}
import com.github.grender.wv_tt.Exchange.{
  ClientAssets,
  EmptyAssets,
  ProcessingStepState
}

import scala.annotation.tailrec

class VectorRecursiveExchange extends AbstractExchange[Vector] {

  def foundSellOrder(
      clients: ClientAssets,
      buyOrder: Order,
      sellOrders: Vector[Order]
  ): (Option[Order], Vector[Order]) = {

    @tailrec
    def iteration(
        sellOrders: Vector[Order],
        filteredSellOrders: Vector[Order]
    ): (Option[Order], Vector[Order]) = {
      if (sellOrders.isEmpty) (None, filteredSellOrders)
      else {
        val sellOrder        = sellOrders.head
        val remainSellOrders = sellOrders.tail
        if (buyOrder.share == sellOrder.share
            && buyOrder.pricePerOne == sellOrder.pricePerOne
            && buyOrder.sharesCount == sellOrder.sharesCount
            && buyOrder.clientName != sellOrder.clientName) {
          val buyerAssets = clients.getOrElse(buyOrder.clientName, EmptyAssets)
          val sellerAssets =
            clients.getOrElse(sellOrder.clientName, EmptyAssets)

          if (sellerAssets.shares(sellOrder.share) >= sellOrder.sharesCount
              && buyerAssets.usd >= sellOrder.fullPrice) {
            (Some(sellOrder), filteredSellOrders ++ remainSellOrders)
          } else {
            iteration(remainSellOrders, filteredSellOrders :+ sellOrder)
          }
        } else {
          iteration(remainSellOrders, filteredSellOrders :+ sellOrder)
        }
      }
    }

    val (orderResult, filteredSellOrders) = iteration(sellOrders, Vector())
    (orderResult, filteredSellOrders)
  }

  override def createProcessingStepState(
      stepState: ProcessingStepState[List]): ProcessingStepState[Vector] =
    stepState.copy(sellOrders = stepState.sellOrders.toVector)
}
