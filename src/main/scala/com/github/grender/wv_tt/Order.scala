package com.github.grender.wv_tt

class Order(val clientName:String,
            val orderType:OrderType.OrderType,
            val share:SharesName.SharesName,
            val pricePerOne:Int,
            val sharesCount:Int) {

  def fullPrice = pricePerOne * sharesCount

  override def toString = s"Order(clientName=$clientName, orderType=$orderType, shareName=$share, pricePerOne=$pricePerOne, sharesCount=$sharesCount, fullPrice=$fullPrice)"

  def isSameOrderValue(other: Order): Boolean =
    clientName == other.clientName &&
      orderType == other.orderType &&
      share == other.share &&
      pricePerOne == other.pricePerOne &&
      sharesCount == other.sharesCount
}
