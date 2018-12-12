package com.github.grender.wv_tt

import java.util.UUID

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

case class OrdersStorage(buyOrders: List[Order], sellOrders: List[Order])
