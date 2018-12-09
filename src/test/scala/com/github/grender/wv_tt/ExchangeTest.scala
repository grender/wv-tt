package com.github.grender.wv_tt

import java.util.UUID

import org.scalatest.{FlatSpec, MustMatchers}
import org.scalatest.Matchers._

class ExchangeTest extends FlatSpec with MustMatchers {
  "addShareInAsset" should "correct doing nothing" in {
    val result = Exchange.addShareAndUsdToAsset(assets(), Shares.A, 0, 0)
    result mustBe assets()
  }

  it should "correct add usd" in {
    val result = Exchange.addShareAndUsdToAsset(assets(), Shares.B, 5, 0)
    result mustBe assets(usd = 15)
  }

  it should "correct reduce usd" in {
    val result = Exchange.addShareAndUsdToAsset(assets(), Shares.C, -5, 0)
    result mustBe assets(usd = 5)
  }

  it should "correct add share" in {
    val result = Exchange.addShareAndUsdToAsset(assets(), Shares.D, 0, 5)
    result mustBe assets(shareD = 12)
  }

  it should "correct reduce share" in {
    val result = Exchange.addShareAndUsdToAsset(assets(), Shares.D, 0, -5)
    result mustBe assets(shareD = 2)
  }

  it should "correct add share/usd" in {
    val result = Exchange.addShareAndUsdToAsset(assets(), Shares.D, 5, 5)
    result mustBe assets(usd = 15, shareD = 12)
  }

  "Trade" should "correct thrown exception" in {
    an[IllegalArgumentException] should be thrownBy Trade(
      order(share = Shares.A),
      order(share = Shares.B))
  }

  "updateClientAssetsWithTrade" should "correct update assets" in {
    val inputAssets = Map(
      "C1" -> assets(usd = 30, shareA = 2),
      "C2" -> assets(usd = 5, shareA = 5)
    )
    val buyOrder =
      order(name = "C1", share = Shares.A, pricePerOne = 5, sharesCount = 5)
    val sellOrder =
      order(name = "C2", share = Shares.A, pricePerOne = 5, sharesCount = 5)
    val newAssets =
      Exchange.updateClientAssetsWithTrade(inputAssets,
                                           Trade(buyOrder, sellOrder))
    newAssets("C1") mustBe assets(usd = 5, shareA = 7)
    newAssets("C2") mustBe assets(usd = 30, shareA = 0)
  }

  "foundSellOrder" should "correct found order and return" in {
    val inputAssets = Map(
      "C1" -> assets(usd = 30, shareA = 2),
      "C2" -> assets(usd = 5, shareA = 5)
    )
    val buyOrder =
      order(name = "C1", share = Shares.A, pricePerOne = 5, sharesCount = 5)
    val sellOrders = List(
      order(uuid = UUID.fromString("00000000-0000-0000-0000-000000000000"),
            name = "C2",
            share = Shares.B,
            pricePerOne = 5,
            sharesCount = 5),
      order(uuid = UUID.fromString("00000000-0000-0000-0000-000000000001"),
            name = "C0",
            share = Shares.A,
            pricePerOne = 5,
            sharesCount = 5),
      order(uuid = UUID.fromString("00000000-0000-0000-0000-000000000002"),
            name = "C2",
            share = Shares.A,
            pricePerOne = 5,
            sharesCount = 5),
      order(uuid = UUID.fromString("00000000-0000-0000-0000-000000000003"),
            name = "C3",
            share = Shares.A,
            pricePerOne = 5,
            sharesCount = 5),
      order(uuid = UUID.fromString("00000000-0000-0000-0000-000000000004"),
            name = "C2",
            share = Shares.B,
            pricePerOne = 5,
            sharesCount = 5)
    )

    val (Some(foundOrder), otherSellOrders) =
      Exchange.foundSellOrder(inputAssets, buyOrder, sellOrders)
    foundOrder.uuid.toString mustBe "00000000-0000-0000-0000-000000000002"
    otherSellOrders.map(_.uuid.toString) should contain allOf ("00000000-0000-0000-0000-000000000000", "00000000-0000-0000-0000-000000000001", "00000000-0000-0000-0000-000000000003", "00000000-0000-0000-0000-000000000004")
  }

  it should "not found order and return" in {
    val inputAssets = Map(
      "C1" -> assets(usd = 30, shareA = 2),
      "C2" -> assets(usd = 5, shareA = 5)
    )
    val notFoundBuyOrders = List(
      order(name = "C1", share = Shares.D, pricePerOne = 5, sharesCount = 5), // no sell orders
      order(name = "C1", share = Shares.B, pricePerOne = 5, sharesCount = 4), // other shares count
      order(name = "C1", share = Shares.B, pricePerOne = 4, sharesCount = 5), // other price
      order(name = "C2", share = Shares.B, pricePerOne = 5, sharesCount = 5), // same client
      order(name = "C1", share = Shares.A, pricePerOne = 5, sharesCount = 50) // client not have share count
    )
    val sellOrders = List(
      order(uuid = UUID.fromString("00000000-0000-0000-0000-000000000000"),
            name = "C2",
            share = Shares.B,
            pricePerOne = 5,
            sharesCount = 5),
      order(uuid = UUID.fromString("00000000-0000-0000-0000-000000000001"),
            name = "C0",
            share = Shares.A,
            pricePerOne = 5,
            sharesCount = 5),
      order(uuid = UUID.fromString("00000000-0000-0000-0000-000000000002"),
            name = "C2",
            share = Shares.A,
            pricePerOne = 5,
            sharesCount = 5),
      order(uuid = UUID.fromString("00000000-0000-0000-0000-000000000003"),
            name = "C3",
            share = Shares.A,
            pricePerOne = 5,
            sharesCount = 5),
      order(uuid = UUID.fromString("00000000-0000-0000-0000-000000000004"),
            name = "C1",
            share = Shares.A,
            pricePerOne = 5,
            sharesCount = 50)
    )

    notFoundBuyOrders.foreach(buyOrder => {
      val (sellOrder, otherSellOrders) =
        Exchange.foundSellOrder(inputAssets, buyOrder, sellOrders)
      sellOrder mustBe None
      otherSellOrders mustBe sellOrders
    })
  }

  "updateState" should "correct found sell order for buy order and apply to assets and get " in {
    val buyOrder = order(
      uuid = UUID.fromString("10000000-0000-0000-0000-000000000000"),
      name = "C1",
      share = Shares.B,
      pricePerOne = 5,
      sharesCount = 6
    )
    val correctSellOrder =
      order(uuid = UUID.fromString("00000000-0000-0000-0000-000000000001"),
            name = "C2",
            share = Shares.B,
            pricePerOne = 5,
            sharesCount = 6)

    val inputAssets = Map(
      "C1" -> assets(usd = 300, shareB = 10),
      "C2" -> assets(usd = 200, shareB = 20),
      "C3" -> assets(usd = 14, shareA = 5),
      "C4" -> assets(usd = 5, shareA = 5)
    )
    val sellOrders = List(
      order(uuid = UUID.fromString("00000000-0000-0000-0000-000000000000"),
            name = "C1",
            share = Shares.A,
            pricePerOne = 50,
            sharesCount = 2),
      correctSellOrder,
      order(uuid = UUID.fromString("00000000-0000-0000-0000-000000000002"),
            name = "C2",
            share = Shares.C,
            pricePerOne = 3,
            sharesCount = 8),
      order(uuid = UUID.fromString("00000000-0000-0000-0000-000000000003"),
            name = "C3",
            share = Shares.D,
            pricePerOne = 4,
            sharesCount = 2),
      order(uuid = UUID.fromString("00000000-0000-0000-0000-000000000004"),
            name = "C2",
            share = Shares.B,
            pricePerOne = 9,
            sharesCount = 8)
    )

    val state = Exchange.updateState(buyOrder)
    val processingStep =
      ProcessingStepState(clientAssets = inputAssets, sellOrders = sellOrders)
    val (newProcessingStep, newAssets) = state.run(processingStep).value

    newProcessingStep.trades should contain only Trade(buyOrder,
                                                       correctSellOrder)
    newAssets("C3") mustBe inputAssets("C3")
    newAssets("C4") mustBe inputAssets("C4")
    (newAssets("C1").usd + buyOrder.fullPrice) mustBe inputAssets("C1").usd
    newAssets("C1").shares(Shares.B) - buyOrder.sharesCount mustBe inputAssets(
      "C1").shares(Shares.B)
  }

  it should "correct send orders to 'withoutSeller' list" in {
    val inputAssets = Map(
      "C1" -> assets(),
      "C2" -> assets(),
      "C3" -> assets(),
      "C4" -> assets()
    )
    val buyOrder = order()

    val state = Exchange.updateState(buyOrder)
    val processingStep =
      ProcessingStepState(clientAssets = inputAssets, sellOrders = List())
    val (newProcessingStep, newAssets) = state.run(processingStep).value

    newProcessingStep.trades mustBe empty
    newAssets mustBe inputAssets
    newProcessingStep.buyOrderWithoutSeller must contain only buyOrder

  }

}
