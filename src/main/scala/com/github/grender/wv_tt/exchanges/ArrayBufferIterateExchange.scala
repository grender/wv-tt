package com.github.grender.wv_tt.exchanges

import com.github.grender.wv_tt.{Order}
import com.github.grender.wv_tt.Exchange.{
  ClientAssets,
  EmptyAssets,
  ProcessingStepState
}

import scala.collection.mutable.ArrayBuffer

class ArrayBufferIterateExchange extends AbstractExchange[ArrayBuffer] {
  override def createProcessingStepState(
      stepState: ProcessingStepState[List]): ProcessingStepState[ArrayBuffer] =
    stepState.copy(sellOrders = ArrayBuffer(stepState.sellOrders: _*))

  override def foundSellOrder(
      clients: ClientAssets,
      buyOrder: Order,
      sellOrders: ArrayBuffer[Order]): (Option[Order], ArrayBuffer[Order]) = {
    var continueSearch                              = true
    var i                                           = 0
    var result: (Option[Order], ArrayBuffer[Order]) = null
    while (continueSearch) {
      if (i >= sellOrders.size) {
        result = (None, sellOrders)
        continueSearch = false
      } else {
        val sellOrder = sellOrders(i)
        if (buyOrder.share == sellOrder.share
            && buyOrder.pricePerOne == sellOrder.pricePerOne
            && buyOrder.sharesCount == sellOrder.sharesCount
            && buyOrder.clientName != sellOrder.clientName) {
          val buyerAssets = clients.getOrElse(buyOrder.clientName, EmptyAssets)
          val sellerAssets =
            clients.getOrElse(sellOrder.clientName, EmptyAssets)

          if (sellerAssets.shares(sellOrder.share) >= sellOrder.sharesCount
              && buyerAssets.usd >= sellOrder.fullPrice) {
            sellOrders.remove(i)
            result = (Some(sellOrder), sellOrders)
            continueSearch = false
          }
        }

      }
      i = i + 1
    }
    result
  }
}
