package com.github.grender.wv_tt

import cats.data.State
import com.github.grender.wv_tt.Exchange.{ClientAssets, EmptyAssets, ProcessingStepState}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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

class ListRecursiveExchange extends AbstractExchange[List] {

  def foundSellOrder(
      clients: ClientAssets,
      buyOrder: Order,
      sellOrders: List[Order]
  ): (Option[Order], List[Order]) = {

    @tailrec
    def iteration(
        sellOrders: List[Order],
        filteredSellOrders: List[Order]
    ): (Option[Order], List[Order]) = {
      if (sellOrders.isEmpty) (None, filteredSellOrders)
      else {
        val sellOrder        = sellOrders.head
        val remainSellOrders = sellOrders.tail
        if (buyOrder.share == sellOrder.share
            && buyOrder.pricePerOne == sellOrder.pricePerOne
            && buyOrder.sharesCount == sellOrder.sharesCount
            && buyOrder.clientName != sellOrder.clientName) {
          val buyerAssets = clients.getOrElse(buyOrder.clientName, EmptyAssets)
          val sellerAssets =
            clients.getOrElse(sellOrder.clientName, EmptyAssets)

          if (sellerAssets.shares(sellOrder.share) >= sellOrder.sharesCount
              && buyerAssets.usd >= sellOrder.fullPrice) {
            (Some(sellOrder), remainSellOrders.reverse :::filteredSellOrders)
          } else {
            iteration(remainSellOrders, sellOrder :: filteredSellOrders)
          }
        } else {
          iteration(remainSellOrders, sellOrder :: filteredSellOrders)
        }
      }
    }

    val (orderResult, filteredSellOrders) = iteration(sellOrders, List())
    (orderResult, filteredSellOrders.reverse)
  }

  override def createProcessingStepState(
      stepState: ProcessingStepState[List]): ProcessingStepState[List] =
    stepState
}

class VectorRecursiveExchange extends AbstractExchange[Vector] {

  def foundSellOrder(
      clients: ClientAssets,
      buyOrder: Order,
      sellOrders: Vector[Order]
  ): (Option[Order], Vector[Order]) = {

    @tailrec
    def iteration(
        sellOrders: Vector[Order],
        filteredSellOrders: Vector[Order]
    ): (Option[Order], Vector[Order]) = {
      if (sellOrders.isEmpty) (None, filteredSellOrders)
      else {
        val sellOrder        = sellOrders.head
        val remainSellOrders = sellOrders.tail
        if (buyOrder.share == sellOrder.share
            && buyOrder.pricePerOne == sellOrder.pricePerOne
            && buyOrder.sharesCount == sellOrder.sharesCount
            && buyOrder.clientName != sellOrder.clientName) {
          val buyerAssets = clients.getOrElse(buyOrder.clientName, EmptyAssets)
          val sellerAssets =
            clients.getOrElse(sellOrder.clientName, EmptyAssets)

          if (sellerAssets.shares(sellOrder.share) >= sellOrder.sharesCount
              && buyerAssets.usd >= sellOrder.fullPrice) {
            (Some(sellOrder), filteredSellOrders ++ remainSellOrders)
          } else {
            iteration(remainSellOrders, filteredSellOrders :+ sellOrder)
          }
        } else {
          iteration(remainSellOrders, filteredSellOrders :+ sellOrder)
        }
      }
    }

    val (orderResult, filteredSellOrders) = iteration(sellOrders, Vector())
    (orderResult, filteredSellOrders)
  }

  override def createProcessingStepState(
      stepState: ProcessingStepState[List]): ProcessingStepState[Vector] =
    stepState.copy(sellOrders = stepState.sellOrders.toVector)
}

class ArrayBufferIterateExchange extends AbstractExchange[ArrayBuffer] {
  override def createProcessingStepState(
      stepState: ProcessingStepState[List]): ProcessingStepState[ArrayBuffer] =
    stepState.copy(sellOrders = ArrayBuffer(stepState.sellOrders: _*))

  override def foundSellOrder(
      clients: ClientAssets,
      buyOrder: Order,
      sellOrders: ArrayBuffer[Order]): (Option[Order], ArrayBuffer[Order]) = {
    var continueSearch                              = true
    var i                                           = 0
    var result: (Option[Order], ArrayBuffer[Order]) = null
    while (continueSearch) {
      if (i >= sellOrders.size) {
        result = (None, sellOrders)
        continueSearch = false
      } else {
        val sellOrder = sellOrders(i)
        if (buyOrder.share == sellOrder.share
            && buyOrder.pricePerOne == sellOrder.pricePerOne
            && buyOrder.sharesCount == sellOrder.sharesCount
            && buyOrder.clientName != sellOrder.clientName) {
          val buyerAssets = clients.getOrElse(buyOrder.clientName, EmptyAssets)
          val sellerAssets =
            clients.getOrElse(sellOrder.clientName, EmptyAssets)

          if (sellerAssets.shares(sellOrder.share) >= sellOrder.sharesCount
              && buyerAssets.usd >= sellOrder.fullPrice) {
            sellOrders.remove(i)
            result = (Some(sellOrder), sellOrders)
            continueSearch = false
          }
        }

      }
      i = i + 1
    }
    result
  }
}

class BufferIterateExchange extends AbstractExchange[mutable.Buffer] {
  override def createProcessingStepState(
                                          stepState: ProcessingStepState[List]): ProcessingStepState[mutable.Buffer] =
    stepState.copy(sellOrders = mutable.Buffer(stepState.sellOrders: _*))

  override def foundSellOrder(
                               clients: ClientAssets,
                               buyOrder: Order,
                               sellOrders: mutable.Buffer[Order]): (Option[Order], mutable.Buffer[Order]) = {
    var continueSearch                              = true
    var i                                           = 0
    var result: (Option[Order], mutable.Buffer[Order]) = null
    while (continueSearch) {
      if (i >= sellOrders.size) {
        result = (None, sellOrders)
        continueSearch = false
      } else {
        val sellOrder = sellOrders(i)
        if (buyOrder.share == sellOrder.share
          && buyOrder.pricePerOne == sellOrder.pricePerOne
          && buyOrder.sharesCount == sellOrder.sharesCount
          && buyOrder.clientName != sellOrder.clientName) {
          val buyerAssets = clients.getOrElse(buyOrder.clientName, EmptyAssets)
          val sellerAssets =
            clients.getOrElse(sellOrder.clientName, EmptyAssets)

          if (sellerAssets.shares(sellOrder.share) >= sellOrder.sharesCount
            && buyerAssets.usd >= sellOrder.fullPrice) {
            sellOrders.remove(i)
            result = (Some(sellOrder), sellOrders)
            continueSearch = false
          }
        }

      }
      i = i + 1
    }
    result
  }
}

class ListSpanExchange extends AbstractExchange[List] {

  def foundSellOrder(
                      clients: ClientAssets,
                      buyOrder: Order,
                      sellOrders: List[Order]
                    ): (Option[Order], List[Order]) = {

    val (notFound,founded)=sellOrders.span(sellOrder => !(buyOrder.share == sellOrder.share
      && buyOrder.pricePerOne == sellOrder.pricePerOne
      && buyOrder.sharesCount == sellOrder.sharesCount
      && buyOrder.clientName != sellOrder.clientName
      && clients.getOrElse(sellOrder.clientName, EmptyAssets).shares(sellOrder.share) >= sellOrder.sharesCount
      && clients.getOrElse(buyOrder.clientName, EmptyAssets).usd >= sellOrder.fullPrice
      ))

    if(founded.isEmpty) {
      (None,sellOrders)
    }else {
      val order = founded.head
      val others = notFound:::founded.tail
      (Some(order), others)
    }
  }

  override def createProcessingStepState(
                                          stepState: ProcessingStepState[List]): ProcessingStepState[List] =
    stepState
}

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
