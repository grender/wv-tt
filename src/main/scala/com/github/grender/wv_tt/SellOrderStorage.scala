package com.github.grender.wv_tt

import scala.collection.mutable

class SellOrderStorage {
  private val _orders:Map[SharesName.SharesName,mutable.ListBuffer[Order]] = Map(SharesName.values.toSeq.map(shareName=> (shareName,mutable.ListBuffer[Order]())):_*)

  def count(shareName: SharesName.SharesName) = _orders(shareName).size

  def countAll = _orders.values.foldRight(0)(_.size+_)

  def isEmpty = countAll == 0

  def add(order:Order) {
    require(order.orderType==OrderType.Sell)
    _orders(order.share) += order
  }

  def remove(order:Order) {
    _orders(order.share) -= order
  }

  def findSellOrderForBuyOrder(buyOrder:Order):Option[Order] = {
    val sellOrders = _orders(buyOrder.share)

    val sellIterator = sellOrders.iterator
    while(sellIterator.hasNext) {
      val sellCandidate = sellIterator.next
      if(buyOrder.sharesCount == sellCandidate.sharesCount
        && buyOrder.pricePerOne == sellCandidate.pricePerOne
        && buyOrder.clientName != sellCandidate.clientName) {
        return Some(sellCandidate)
      }
    }
    None
  }

  override def toString: String = s"Sell orders count: $countAll"

}