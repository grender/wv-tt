package com.github.grender.wv_tt
import scala.collection.mutable

class BuyOrderStorage {

  private val _orders = mutable.Queue[Order]()

  def count = _orders.size

  def add(order:Order) {
    require(order.orderType==OrderType.Buy)
    _orders.enqueue(order)
  }

  def isEmpty = _orders.isEmpty

  def takeOrder : Option[Order] = _orders.isEmpty match {
    case true => None
    case false => Some(_orders.dequeue)
  }

  override def toString: String = s"Buy orders count: $count"

}
