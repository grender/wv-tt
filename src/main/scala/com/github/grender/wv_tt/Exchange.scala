package com.github.grender.wv_tt

import cats.data.State

object Exchange {
  val EmptyAssets = Assets(0,
                           Map(
                             Shares.A -> 0,
                             Shares.B -> 0,
                             Shares.C -> 0,
                             Shares.D -> 0
                           ))

  type ClientAssets = Map[String, Assets]
  val ClientAssets = Map[String, Assets] _

  type FullState[T] = State[ProcessingStepState, T]

  def updateState(buyOrder: Order): FullState[ClientAssets] = {
    State[ProcessingStepState, ClientAssets] { oldState =>
      {
        val (sellOrderO, remainSellOrders) =
          foundSellOrder(oldState.clientAssets, buyOrder, oldState.sellOrders)
        sellOrderO match {
          case Some(sellOrder) => {
            val trade = Trade(buyOrder, sellOrder)
            val newClients =
              updateClientAssetsWithTrade(oldState.clientAssets, trade)

            (
              oldState.copy(
                trades = trade :: oldState.trades,
                clientAssets = newClients,
                sellOrders = remainSellOrders
              ),
              newClients
            )

          }
          case None => {
            val newState = oldState.copy(
              buyOrderWithoutSeller = buyOrder :: oldState.buyOrderWithoutSeller
            )
            (newState, oldState.clientAssets)
          }
        }
      }
    }
  }

  def isCorrectBuySellOrderPair(clients: ClientAssets,
                                buyOrder: Order,
                                sellOrder: Order): Boolean = {
    if (buyOrder.share == sellOrder.share
        && buyOrder.pricePerOne == sellOrder.pricePerOne
        && buyOrder.sharesCount == sellOrder.sharesCount
        && buyOrder.clientName != sellOrder.clientName) {
      val buyerAssets  = clients.getOrElse(buyOrder.clientName, EmptyAssets)
      val sellerAssets = clients.getOrElse(sellOrder.clientName, EmptyAssets)

      if (sellerAssets.shares(sellOrder.share) >= sellOrder.sharesCount
          && buyerAssets.usd >= sellOrder.fullPrice) {
        true
      } else {
        false
      }
    } else {
      false
    }
  }

  def foundSellOrder(
      clients: ClientAssets,
      buyOrder: Order,
      sellOrders: List[Order]
  ): (Option[Order], List[Order]) = {

    val (nonSuitableOrders, foundSellOrderAndOtherOrders) =
      sellOrders.span(sellOrder =>
        !isCorrectBuySellOrderPair(clients, buyOrder, sellOrder))

    foundSellOrderAndOtherOrders match {
      case Nil => (None, nonSuitableOrders)
      case sellOrder :: otherOrders =>
        (Some(sellOrder), nonSuitableOrders ::: otherOrders)
    }
  }

  def addShareAndUsdToAsset(assets: Assets,
                            share: Shares.Shares,
                            addUsd: Int,
                            addSharesCount: Int) = {
    val newShares = assets.shares.map({
      case (`share`, oldSharesCount) =>
        share -> (oldSharesCount + addSharesCount)
      case other => other
    })
    assets.copy(usd = assets.usd + addUsd, shares = newShares)
  }

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
}
