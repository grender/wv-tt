package com.github.grender.wv_tt

import scala.collection.mutable
import scala.io.Source

object Main extends App {

  val sellOrderStorage = new SellOrderStorage
  val buyOrderStorage = new BuyOrderStorage

  val clients = FileUtils.loadClients(Source.fromFile("clients.txt")).toList
  FileUtils.loadOrders(Source.fromFile("orders.txt")).foreach(order => {
    order.orderType match {
      case OrderType.Sell => sellOrderStorage.add(order)
      case OrderType.Buy => buyOrderStorage.add(order)
      case _ => new Exception("Invalid state")
    }
  })

  TradeRunner.run(clients, buyOrderStorage, sellOrderStorage)
  FileUtils.writeClientsResult("result.txt",clients)
}
