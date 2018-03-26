package com.github.grender.wv_tt

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


  def canEqual(other: Any): Boolean = other.isInstanceOf[Balance]

  override def equals(other: Any): Boolean = other match {
    case that: Balance =>
      (that canEqual this) &&
        _balance == that._balance
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(_balance)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

