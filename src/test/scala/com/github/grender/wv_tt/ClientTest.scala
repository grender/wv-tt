package com.github.grender.wv_tt

import org.scalatest.FlatSpec

class ClientTest extends FlatSpec {
  "Client" should "created with correct balance" in {
    val client = new Client("C",1,2,3,4,5)

    assert(client.usdBalance.get == 1)
    assert(client.shares(SharesName.A).get == 2)
    assert(client.shares(SharesName.B).get == 3)
    assert(client.shares(SharesName.C).get == 4)
    assert(client.shares(SharesName.D).get == 5)


  }

  it should "correct check same client value" in {
    val client = new Client("C",1,2,3,4,5)
    assert( client.isSameClientValue(new Client("C",1,2,3,4,5)))
    assert(!client.isSameClientValue(new Client("A",1,2,3,4,5)))
    assert(!client.isSameClientValue(new Client("C",2,2,3,4,5)))
    assert(!client.isSameClientValue(new Client("C",1,3,3,4,5)))
    assert(!client.isSameClientValue(new Client("C",1,2,4,4,5)))
    assert(!client.isSameClientValue(new Client("C",1,2,3,5,5)))
    assert(!client.isSameClientValue(new Client("C",1,2,3,4,6)))
  }
}
