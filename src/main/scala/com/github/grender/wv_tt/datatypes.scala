package com.github.grender.wv_tt

import java.util.UUID

import com.github.grender.wv_tt.Exchange.ClientAssets

object Shares extends Enumeration {
  type Shares = Value
  val A, B, C, D = Value
}

case class Assets(usd: Int, shares: Map[Shares.Shares, Int])

case class Order(
    uuid: UUID,
    clientName: String,
    share: Shares.Shares,
    pricePerOne: Int,
    sharesCount: Int
) {
  def fullPrice = pricePerOne * sharesCount
}

case class Trade(
    buyOrder: Order,
    sellOrder: Order
) {
  require(buyOrder.share == sellOrder.share)
  require(buyOrder.sharesCount == sellOrder.sharesCount)
  require(buyOrder.pricePerOne == sellOrder.pricePerOne)
}

case class ProcessingStepState(
    clientAssets: ClientAssets,
    sellOrders: List[Order],
    buyOrderWithoutSeller: List[Order] = List(),
    trades: List[Trade] = List()
)

case class OrdersStorage(buyOrders: List[Order], sellOrders: List[Order])
