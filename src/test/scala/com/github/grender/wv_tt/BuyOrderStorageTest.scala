package com.github.grender.wv_tt

import org.scalatest.FlatSpec

class BuyOrderStorageTest extends FlatSpec {
  val buyOrderStorage = new BuyOrderStorage
  val orderSell = new Order("C1", OrderType.Sell, SharesName.A, 1, 1)
  val orderBuy1 = new Order("C2", OrderType.Buy, SharesName.A, 1, 1)
  val orderBuy2 = new Order("C3", OrderType.Buy, SharesName.A, 1, 1)

  "BuyOrderStorage" should "correct add element" in {
    buyOrderStorage.add(orderBuy1)
    assert(buyOrderStorage.count == 1)

    intercept[Exception] {
      buyOrderStorage.add(orderSell)
    }

    buyOrderStorage.add(orderBuy2)
    assert(buyOrderStorage.count == 2)
  }

  it should "correct deque order" in {
    val testOrderOption1 = buyOrderStorage.takeOrder

    assert(testOrderOption1.isDefined)
    assert(buyOrderStorage.count == 1)

    val testOrderOption2 = buyOrderStorage.takeOrder

    assert(testOrderOption2.isDefined)
    assert(buyOrderStorage.count == 0)

    val listOrderBuy = List(orderBuy1, orderBuy2)
    assert(listOrderBuy.contains(testOrderOption1.get)
      && listOrderBuy.contains(testOrderOption2.get)
      && testOrderOption1.get!=testOrderOption2.get)
  }
}
