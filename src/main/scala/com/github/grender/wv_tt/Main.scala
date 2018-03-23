package com.github.grender.wv_tt

import scala.collection.mutable
import scala.io.Source

object Main extends App {

  val orderStorage = new OrderStorage
  var unprocessedBuyOrders = List[Order]()

  val clients = FileUtils.getLineDelemiteredByTab("clients.txt").map({
    case Array(name, usdBalance, aBalance, bBalance, cBalance, dBalance) => name -> new Client(name, usdBalance.toInt, aBalance.toInt, bBalance.toInt, cBalance.toInt, dBalance.toInt)
    case _ => throw new Exception("Error parsing clients.txt file")
  }).toMap

  FileUtils.getLineDelemiteredByTab("orders.txt").foreach({
    case Array(clientName, orderType, shareName, pricePerOne, sharesCount) => {
      val order = new Order(clientName, OrderType.fromLetter(orderType), SharesName.fromLetter(shareName), pricePerOne.toInt, sharesCount.toInt)
      orderStorage.add(order)
    }
    case _ => throw new Exception("Error parsing orders.txt file")
  })

  println(
    s"""--- Start ---
      |Clients: $clients
      |Orders stat: $orderStorage
    """.stripMargin)

  var tradeCount = 0

  while(!orderStorage.isNoneBuyOrders) {
    val buyOrderOption = orderStorage.takeBuyOrder
    val sellOrderOption = buyOrderOption match {
      case None => None
      case Some(buyOrder) => orderStorage.takeSellOrderForBuyOrder(buyOrder)
    }

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
      }
      case (Some(buyOrder), None) => unprocessedBuyOrders = buyOrder :: unprocessedBuyOrders
      case _=> throw new Exception("Invalid state")
    }
  }
  println(s"Unprocessed buy orders: ${unprocessedBuyOrders.length}")
  println(
    s"""--- End ---
       |Clients: $clients
       |Orders stat: $orderStorage
    """.stripMargin)

}
