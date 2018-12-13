package com.github.grender.wv_tt.exchanges

import com.github.grender.wv_tt.{Order}
import com.github.grender.wv_tt.Exchange.{
  ClientAssets,
  EmptyAssets,
  ProcessingStepState
}

import scala.annotation.tailrec

class ListRecursiveExchange extends AbstractExchange[List] {

  def foundSellOrder(
      clients: ClientAssets,
      buyOrder: Order,
      sellOrders: List[Order]
  ): (Option[Order], List[Order]) = {

    @tailrec
    def iteration(
        sellOrders: List[Order],
        filteredSellOrders: List[Order]
    ): (Option[Order], List[Order]) = {
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
            (Some(sellOrder), remainSellOrders.reverse ::: filteredSellOrders)
          } else {
            iteration(remainSellOrders, sellOrder :: filteredSellOrders)
          }
        } else {
          iteration(remainSellOrders, sellOrder :: filteredSellOrders)
        }
      }
    }

    val (orderResult, filteredSellOrders) = iteration(sellOrders, List())
    (orderResult, filteredSellOrders.reverse)
  }

  override def createProcessingStepState(
      stepState: ProcessingStepState[List]): ProcessingStepState[List] =
    stepState
}
