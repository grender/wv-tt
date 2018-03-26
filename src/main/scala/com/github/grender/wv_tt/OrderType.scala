package com.github.grender.wv_tt

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
