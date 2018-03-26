package com.github.grender.wv_tt

class Client(val name:String,
             initialUsd:Int,
             initialA:Int,
             initialB:Int,
             initialC:Int,
             initialD:Int
            ) {

  def isSameClientValue(other: Client):Boolean =
    name == other.name &&
      usdBalance == other.usdBalance &&
      shares(SharesName.A) == other.shares(SharesName.A) &&
      shares(SharesName.B) == other.shares(SharesName.B) &&
      shares(SharesName.C) == other.shares(SharesName.C) &&
      shares(SharesName.D) == other.shares(SharesName.D)

  val usdBalance = new Balance(initialUsd)

  val shares = Map (
    SharesName.A -> new Balance(initialA),
    SharesName.B -> new Balance(initialB),
    SharesName.C -> new Balance(initialC),
    SharesName.D -> new Balance(initialD)
  )

  override def toString = s"Client(name=$name, usd=$usdBalance, sharesA=${shares(SharesName.A)}, sharesB=${shares(SharesName.B)}, sharesC=${shares(SharesName.C)}, sharesD=${shares(SharesName.D)})"

  def forFile = s"$name\t$usdBalance\t${shares(SharesName.A)}\t${shares(SharesName.B)}\t${shares(SharesName.C)}\t${shares(SharesName.D)}"
}
