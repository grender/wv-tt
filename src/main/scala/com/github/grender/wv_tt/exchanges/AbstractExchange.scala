package com.github.grender.wv_tt.exchanges

import cats.data.State
import com.github.grender.wv_tt.{Exchange, Order, Trade}
import com.github.grender.wv_tt.Exchange.{ClientAssets, ProcessingStepState}

trait AbstractExchange[T[_]] {

  type FullState[RES] = State[ProcessingStepState[T], RES]

  def createProcessingStepState(
      stepState: ProcessingStepState[List]): ProcessingStepState[T]

  def updateState(buyOrder: Order): FullState[ClientAssets] = {
    State[ProcessingStepState[T], ClientAssets] { oldState =>
      {
        val (sellOrderO, remainSellOrders) =
          foundSellOrder(oldState.clientAssets, buyOrder, oldState.sellOrders)
        sellOrderO match {
          case Some(sellOrder) => {
            val trade = Trade(buyOrder, sellOrder)
            val newClients =
              Exchange.updateClientAssetsWithTrade(oldState.clientAssets, trade)

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

  def foundSellOrder(
      clients: ClientAssets,
      buyOrder: Order,
      sellOrders: T[Order]
  ): (Option[Order], T[Order])
}
