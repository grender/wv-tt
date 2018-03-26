package com.github.grender.wv_tt

import scala.collection.mutable

case class TradeRunResult(tradeCount: Int, noSellerBuyOrders:List[Order])

object TradeRunner {
  def run(clientsIterable: Iterable[Client],buyOrderStorage:BuyOrderStorage, sellOrderStorage: SellOrderStorage)= {
    val clients = clientsIterable.map(c=>c.name -> c).toMap
    val noSellerBuyOrders = mutable.MutableList[Order]()

    println(
      s"""--- Start ---
         |Clients: $clients
         |$buyOrderStorage
         |$sellOrderStorage
    """.stripMargin)

    var tradeCount = 0

    while (!buyOrderStorage.isEmpty) {
      val buyOrderOption = buyOrderStorage.takeOrder
      val sellOrderOption = buyOrderOption.flatMap(sellOrderStorage.findSellOrderForBuyOrder(_))

      (buyOrderOption, sellOrderOption) match {
        case (Some(buyOrder), Some(sellOrder)) => {
          val buyer = clients(buyOrder.clientName)
          val seller = clients(sellOrder.clientName)

          val usdFullPrice = buyOrder.fullPrice
          val sharesCount = buyOrder.sharesCount

          tradeCount += 1

          println(s"Trade $tradeCount share:${buyOrder.share} ${buyer.name}->${seller.name} count:${buyOrder.sharesCount} fullPrice:${usdFullPrice}")

          buyer.usdBalance.reduce(usdFullPrice)
          seller.shares(sellOrder.share).reduce(sharesCount)

          buyer.shares(sellOrder.share).add(sharesCount)
          seller.usdBalance.add(usdFullPrice)

          sellOrderStorage.remove(sellOrder)
        }
        case (Some(buyOrder), None) => noSellerBuyOrders += buyOrder
        case _ => throw new Exception("Invalid state")
      }
    }

    println(s"Trade count: $tradeCount")
    println(s"No seller buy orders: ${noSellerBuyOrders.length}")

    println(
      s"""--- End ---
         |$buyOrderStorage
         |$sellOrderStorage
    """.stripMargin)

    TradeRunResult(tradeCount, noSellerBuyOrders.toList)
  }
}
