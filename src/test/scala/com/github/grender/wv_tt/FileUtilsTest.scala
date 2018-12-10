package com.github.grender.wv_tt

import java.util.UUID

import org.scalatest.{FlatSpec, MustMatchers}

import scala.io.Source

class FileUtilsTest extends FlatSpec with MustMatchers {
  "FileUtils" should "correct load clients" in {
    val correctClients = Map(
      "C1" -> assets(1000, 130, 240, 760, 320),
      "C2" -> assets(4350, 370, 120, 950, 560)
    )

    val loadedClients =
      FileUtils.loadClients(Source.fromResource("fileUtils/clients.txt"))
    loadedClients mustBe correctClients
  }

  it should "correct load orders" in {
    val zeroUUID = UUID.fromString("00000000-0000-0000-0000-000000000000")

    val correctOrdersStorage = OrdersStorage(
      List(
        order("C8", Shares.C, 10, 7, uuid = zeroUUID),
        order("C2", Shares.C, 11, 5, uuid = zeroUUID),
        order("C1", Shares.C, 14, 7, uuid = zeroUUID)
      ),
      List(
        order("C2", Shares.D, 13, 1, uuid = zeroUUID),
        order("C6", Shares.A, 12, 9, uuid = zeroUUID),
        order("C10", Shares.B, 14, 7, uuid = zeroUUID)
      )
    )

    val loadedOrders =
      FileUtils.loadOrders(Source.fromResource("fileUtils/orders.txt"))

    val ordersWithCleanUdids = loadedOrders.copy(
      buyOrders = loadedOrders.buyOrders.map(_.copy(uuid = zeroUUID)),
      sellOrders = loadedOrders.sellOrders.map(_.copy(uuid = zeroUUID))
    )

    ordersWithCleanUdids mustBe correctOrdersStorage
  }

  it should "get correct file content for assets" in {
    val inputAssets = Map(
      "C1" -> assets(usd = 30, 1, 2, 3, 4),
      "C3" -> assets(usd = 5, 10, 20, 30, 40),
      "C2" -> assets(usd = 1, 2, 3, 4, 5)
    )
    val content = FileUtils.getClientFileContent(inputAssets)
    val correct = "C1 30 1 2 3 4\n".replace(" ", "\t") +
      "C2 1 2 3 4 5\n".replace(" ", "\t") +
      "C3 5 10 20 30 40".replace(" ", "\t")
    content mustBe correct
  }
}
