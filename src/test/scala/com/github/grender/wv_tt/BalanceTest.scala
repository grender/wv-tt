package com.github.grender.wv_tt

import org.scalatest.FlatSpec

class BalanceTest extends FlatSpec {
  "Balance" should "created with balance 0" in {
    assert((new Balance).get==0)
  }

  it should "add/reduce correct" in {
    val b = new Balance

    b.add(10)
    assert(b.get==10)

    b.reduce(5)
    assert(b.get==5)
  }

  it should "correct check equals" in {
    assert(new Balance(1)==new Balance(1))
    assert(new Balance(1)!=new Balance(2))
  }

}
