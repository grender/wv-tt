package com.github.grender.wv_tt

import org.scalatest.FlatSpec

import scala.io.Source

class FileUtilsTest extends FlatSpec {
  "FileUtils" should "load orders" in {
    val correctList = List(
      new Order("C8", OrderType.Buy, SharesName.C, 15 ,4),
      new Order("C2", OrderType.Sell, SharesName.C, 14 ,5)
    )

    val loadedOrderList = FileUtils.loadOrders(Source.fromResource("fileUtils/orders.txt")).toList

    val correctOrdersCount = correctList.zip(loadedOrderList).map(t=>t._1.isSameOrderValue(t._2)).filter(_==true).length
    assert(correctList.size == correctOrdersCount)
  }

  it should "load clients" in {
    val correctList = List(
      new Client("C1", 1000, 130, 240, 760, 320),
      new Client("C2", 4350, 370, 120, 950, 560)
    )

    val loadedOrderList = FileUtils.loadClients(Source.fromResource("fileUtils/clients.txt")).toList

    val correctOrdersCount = correctList.zip(loadedOrderList).map(t=>t._1.isSameClientValue(t._2)).filter(_==true).length
    assert(correctList.size == correctOrdersCount)
  }

  it should "correct serialize clients" in {
    val clients = List(
      new Client("C1", 1000, 130, 240, 760, 320),
      new Client("C2", 4350, 370, 120, 950, 560)
    )
    assert(FileUtils.getClientFileContent(clients)==Source.fromResource("fileUtils/clients.txt").mkString)
  }
}