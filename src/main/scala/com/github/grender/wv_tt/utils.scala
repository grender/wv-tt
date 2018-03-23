package com.github.grender.wv_tt

import scala.collection.mutable
import scala.io.Source

class Balance(initialBalance:Int = 0) {

  private var _balance = initialBalance

  def add(addVal:Int) {
    _balance += addVal
  }

  def reduce(reduceVal:Int) {
    _balance -= reduceVal
  }

  def get = _balance


  override def toString = _balance.toString
}

class Client(val name:String,
             initialUsd:Int,
             initialA:Int,
             initialB:Int,
             initialC:Int,
             initialD:Int
            ) {
  val usdBalance = new Balance(initialUsd)

  val shares = Map (
    SharesName.A -> new Balance(initialA),
    SharesName.B -> new Balance(initialB),
    SharesName.C -> new Balance(initialC),
    SharesName.D -> new Balance(initialD)
  )
  
  override def toString = s"Client(name=$name, usd=$usdBalance, sharesA=${shares(SharesName.A)}, sharesB=${shares(SharesName.B)}, sharesC=${shares(SharesName.C)}, sharesD=${shares(SharesName.D)})"
}

object OrderType extends Enumeration {
  type OrderType = Value

  val Buy,Sell = Value

  def fromLetter(s:String):OrderType = {
    s match {
      case "s"=>Sell
      case "b"=>Buy
      case _=> throw new Exception("Invalid orderType string value")
    }
  }
}

object SharesName extends Enumeration {
  type SharesName = Value

  val A,B,C,D = Value

  def fromLetter(s:String):SharesName = {
    s match {
      case "A" => A
      case "B" => B
      case "C" => C
      case "D" => D
      case _=> throw new Exception("Invalid sharesName string value")
    }
  }
}

class Order(val clientName:String,
            val orderType:OrderType.OrderType,
            val share:SharesName.SharesName,
            val pricePerOne:Int,
            val sharesCount:Int) {

  def fullPrice = pricePerOne * sharesCount


  override def toString = s"Order(clientName=$clientName, orderType=$orderType, shareName=$share, pricePerOne=$pricePerOne, sharesCount=$sharesCount, fullPrice=$fullPrice)"
}

object FileUtils {
  def getLineDelemiteredByTab(fileName: String) = Source.fromFile(fileName, "UTF-8").getLines.map(_.split("\t"))
}



class OrderStorage {

  private var _orderBuy = List[Order]()
  private val _orderSell:mutable.Map[SharesName.SharesName,List[Order]] = mutable.Map(SharesName.values.toSeq.map(shareName=> (shareName,List[Order]())):_*)

  def add(order:Order) {
    order.orderType match {
      case OrderType.Buy=> _orderBuy = order :: _orderBuy
      case OrderType.Sell=> _orderSell(order.share) = order :: _orderSell(order.share)
    }
  }

  def isNoneBuyOrders = _orderBuy.isEmpty

  def takeBuyOrder : Option[Order] = _orderBuy match {
    case order :: otherOrders => {
      _orderBuy = otherOrders
      Some(order)
    }
    case Nil => None
  }

  def takeSellOrderForBuyOrder(buyOrder:Order):Option[Order] = {
    val sellOrders = _orderSell(buyOrder.share)

    // TODO: rewrite to more optinal solution if need
    val (sellCandidates,otherOrders) = sellOrders.span(sellCandidate=>
      buyOrder.sharesCount == sellCandidate.sharesCount
        && buyOrder.pricePerOne == sellCandidate.pricePerOne
        && buyOrder.clientName != sellCandidate.clientName
    )

    sellCandidates match {
      case foundOrder :: Nil => {
        _orderSell(buyOrder.share) = otherOrders
        Some(foundOrder)
      }
      case foundOrder :: otherCandidates => {
        _orderSell(buyOrder.share) = otherOrders ::: otherCandidates
        Some(foundOrder)
      }
      case Nil => None
    }
  }

  override def toString: String =
    s"""
       |Buy orders: ${_orderBuy.length} Sell orders: ${_orderSell.values.foldRight(0)(_.size+_)}
    """.stripMargin

}