package com.github.grender

import java.util.UUID

import scala.util.Random

package object wv_tt {
  def assets(usd: Int = 10,
             shareA: Int = 1,
             shareB: Int = 3,
             shareC: Int = 5,
             shareD: Int = 7) = Assets(
    usd,
    Map(
      Shares.A -> shareA,
      Shares.B -> shareB,
      Shares.C -> shareC,
      Shares.D -> shareD
    )
  )

  def order(
      name: String = Random.nextString(5),
      share: Shares.Shares = Shares.A,
      pricePerOne: Int = 1,
      sharesCount: Int = 7,
      uuid: UUID = UUID.randomUUID()
  ) = Order(
    uuid = uuid,
    clientName = name,
    share = share,
    pricePerOne = pricePerOne,
    sharesCount = sharesCount
  )
}
