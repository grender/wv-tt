package com.github.grender.wv_tt

import org.scalatest.FlatSpec

class OrderTypeTest extends FlatSpec {
  "OrderType" should "correct create from string" in {

    assert(OrderType.fromLetter("s")==OrderType.Sell)
    assert(OrderType.fromLetter("b")==OrderType.Buy)

    val exception = intercept[Exception] {
      OrderType.fromLetter("xxx")
    }
    assert(exception.getMessage.startsWith("Invalid orderType string value"))
  }
}