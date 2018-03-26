package com.github.grender.wv_tt

import org.scalatest.FlatSpec

import scala.io.Source

class TradeRunnerTest extends FlatSpec {
  "TradeRunner" should "correct do trade" in {
    val clientsIterable = FileUtils.loadClients(Source.fromResource("tradeRunner/clients.txt"))
    val orders = FileUtils.loadOrders(Source.fromResource("tradeRunner/orders.txt"))

    val buyOrderStorage = new BuyOrderStorage
    val sellOrderStorage = new SellOrderStorage

    orders.foreach(order => {
      order.orderType match {
        case OrderType.Sell => sellOrderStorage.add(order)
        case OrderType.Buy => buyOrderStorage.add(order)
        case _ => new Exception("Invalid state")
      }
    })

    val result = TradeRunner.run(clientsIterable,buyOrderStorage,sellOrderStorage)
    assert(result.tradeCount == 2)
    assert(result.noSellerBuyOrders.length==1)

    val correctClientsIterable = FileUtils.loadClients(Source.fromResource("tradeRunner/result.txt"))

    clientsIterable.foreach(newValueClient=> {
      val correctClient = correctClientsIterable.filter(_.name == newValueClient.name).head
      assert(newValueClient.isSameClientValue(correctClient),correctClient.name)
    })
  }
}