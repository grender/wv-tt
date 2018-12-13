package com.github.grender.wv_tt.exchanges

import com.github.grender.wv_tt.{Order}
import com.github.grender.wv_tt.Exchange.{
  ClientAssets,
  EmptyAssets,
  ProcessingStepState
}

class ListSpanExchange extends AbstractExchange[List] {

  def foundSellOrder(
      clients: ClientAssets,
      buyOrder: Order,
      sellOrders: List[Order]
  ): (Option[Order], List[Order]) = {

    val (notFound, founded) = sellOrders.span(
      sellOrder =>
        !(buyOrder.share == sellOrder.share
          && buyOrder.pricePerOne == sellOrder.pricePerOne
          && buyOrder.sharesCount == sellOrder.sharesCount
          && buyOrder.clientName != sellOrder.clientName
          && clients
            .getOrElse(sellOrder.clientName, EmptyAssets)
            .shares(sellOrder.share) >= sellOrder.sharesCount
          && clients
            .getOrElse(buyOrder.clientName, EmptyAssets)
            .usd >= sellOrder.fullPrice))

    if (founded.isEmpty) {
      (None, sellOrders)
    } else {
      val order  = founded.head
      val others = notFound ::: founded.tail
      (Some(order), others)
    }
  }

  override def createProcessingStepState(
      stepState: ProcessingStepState[List]): ProcessingStepState[List] =
    stepState
}
