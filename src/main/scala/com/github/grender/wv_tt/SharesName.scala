package com.github.grender.wv_tt

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
