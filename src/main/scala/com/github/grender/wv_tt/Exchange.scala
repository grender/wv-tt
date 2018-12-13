package com.github.grender.wv_tt

object Exchange {
  type ClientAssets = Map[String, Assets]

  case class ProcessingStepState[T[_]](
      clientAssets: ClientAssets,
      sellOrders: T[Order],
      buyOrderWithoutSeller: List[Order] = List(),
      trades: List[Trade] = List()
  )

  val EmptyAssets = Assets(0,
                           Map(
                             Shares.A -> 0,
                             Shares.B -> 0,
                             Shares.C -> 0,
                             Shares.D -> 0
                           ))
  val ClientAssets = Map[String, Assets] _

  def updateClientAssetsWithTrade(
      clients: ClientAssets,
      trade: Trade
  ): ClientAssets = {
    val price       = trade.buyOrder.fullPrice
    val share       = trade.buyOrder.share
    val sharesCount = trade.sellOrder.sharesCount
    val buyer       = trade.buyOrder.clientName
    val seller      = trade.sellOrder.clientName

    clients.map {
      case (`buyer`, assets) =>
        buyer -> addShareAndUsdToAsset(assets, share, -price, +sharesCount)
      case (`seller`, assets) =>
        seller -> addShareAndUsdToAsset(assets, share, +price, -sharesCount)
      case other => other
    }
  }

  def addShareAndUsdToAsset(
      assets: Assets,
      share: Shares.Shares,
      addUsd: Int,
      addSharesCount: Int
  ) = {
    val newShares = assets.shares.map({
      case (`share`, oldSharesCount) =>
        share -> (oldSharesCount + addSharesCount)
      case other => other
    })
    assets.copy(usd = assets.usd + addUsd, shares = newShares)
  }

}
