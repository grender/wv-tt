package com.github.grender.wv_tt

import org.scalatest.FlatSpec

import scala.collection.mutable
import scala.util.Random

class SellOrderStorageTest extends FlatSpec {

  "SellOrderStorage" should "correct add/remove" in {
    val orderSell1 = new Order("C1", OrderType.Sell, SharesName.A, 1, 1)
    val orderSell2 = new Order("C3", OrderType.Sell, SharesName.B, 1, 1)
    val orderSell3 = new Order("C3", OrderType.Sell, SharesName.B, 1, 1)
    val orderBuy = new Order("C2", OrderType.Buy, SharesName.A, 1, 1)
    val orderStorage = new SellOrderStorage

    assert(orderStorage.count(SharesName.A) == 0)
    assert(orderStorage.count(SharesName.B) == 0)
    assert(orderStorage.countAll == 0)

    orderStorage.add(orderSell1)

    assert(orderStorage.count(SharesName.A) == 1)
    assert(orderStorage.count(SharesName.B) == 0)
    assert(orderStorage.countAll == 1)

    intercept[Exception] {
      orderStorage.add(orderBuy)
    }

    assert(orderStorage.count(SharesName.A) == 1)
    assert(orderStorage.count(SharesName.B) == 0)
    assert(orderStorage.countAll == 1)

    orderStorage.add(orderSell2)
    assert(orderStorage.count(SharesName.A) == 1)
    assert(orderStorage.count(SharesName.B) == 1)
    assert(orderStorage.countAll == 2)

    orderStorage.add(orderSell3)
    assert(orderStorage.count(SharesName.A) == 1)
    assert(orderStorage.count(SharesName.B) == 2)
    assert(orderStorage.countAll == 3)



    orderStorage.remove(orderSell3)
    assert(orderStorage.count(SharesName.A) == 1)
    assert(orderStorage.count(SharesName.B) == 1)
    assert(orderStorage.countAll == 2)

    orderStorage.remove(orderSell2)
    assert(orderStorage.count(SharesName.A) == 1)
    assert(orderStorage.count(SharesName.B) == 0)
    assert(orderStorage.countAll == 1)

    orderStorage.remove(orderSell1)
    assert(orderStorage.count(SharesName.A) == 0)
    assert(orderStorage.count(SharesName.B) == 0)
    assert(orderStorage.countAll == 0)
  }

  {
    val cases = List(
      (new Order("C1", OrderType.Buy, SharesName.A, 1, 1), List(new Order("C1", OrderType.Sell, SharesName.A, 1, 1)), None),
      (new Order("C1", OrderType.Buy, SharesName.A, 1, 1), List(new Order("C2", OrderType.Sell, SharesName.A, 1, 1)), Some(new Order("C2", OrderType.Sell, SharesName.A, 1, 1))),
      (new Order("C1", OrderType.Buy, SharesName.A, 1, 1), List(new Order("C2", OrderType.Sell, SharesName.A, 1, 2)), None),
      (new Order("C1", OrderType.Buy, SharesName.A, 1, 1), List(new Order("C2", OrderType.Sell, SharesName.A, 1, 2)), None),
      (new Order("C1", OrderType.Buy, SharesName.B, 1, 1), List(new Order("C2", OrderType.Sell, SharesName.A, 1, 1), new Order("C2", OrderType.Sell, SharesName.B, 1, 1)), Some(new Order("C2", OrderType.Sell, SharesName.B, 1, 1)))
    )
    var caseNum = 0
    cases.foreach({
      case (buyOrder, listOfSellOrders, foundResult) => {
        it should s"correct find order (single coorect) case: $caseNum" in {
          val orderStorage = new SellOrderStorage
          listOfSellOrders.foreach(orderStorage.add(_))
          val candidate = orderStorage.findSellOrderForBuyOrder(buyOrder)
          (candidate, foundResult) match {
            case (None, None) => succeed
            case (Some(a), Some(b)) if a.isSameOrderValue(b) => succeed
            case _ => fail(s"Incorrect (candidate, foundResult): ${(candidate, foundResult)}")
          }
        }
        caseNum += 1
      }
    })
  }

  it should "correct find order from (one of correct)" in {
    val buyOrder = new Order("C1", OrderType.Buy, SharesName.A, 1, 1)
    val orderStorage = new SellOrderStorage
    val notSelectedSellOrder = List (
      new Order("C1", OrderType.Sell, SharesName.A, 1, 1),
      new Order("C2", OrderType.Sell, SharesName.A, 1, 2),
      new Order("C2", OrderType.Sell, SharesName.A, 2, 1),
      new Order("C2", OrderType.Sell, SharesName.B, 1, 1)
    )
    val correctSellOrders = List(
      new Order("C2", OrderType.Sell, SharesName.A, 1, 1),
      new Order("C3", OrderType.Sell, SharesName.A, 1, 1),
      new Order("C4", OrderType.Sell, SharesName.A, 1, 1))

    Random.shuffle(notSelectedSellOrder ::: correctSellOrders).foreach(orderStorage.add(_))

    val candidate = orderStorage.findSellOrderForBuyOrder(buyOrder)
    assert(candidate.isDefined && correctSellOrders.contains(candidate.get) && !notSelectedSellOrder.contains(candidate.get))
  }
}