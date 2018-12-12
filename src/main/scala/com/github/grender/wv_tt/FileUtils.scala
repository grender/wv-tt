package com.github.grender.wv_tt

import java.io.{File, PrintWriter}
import java.util.UUID

import com.github.grender.wv_tt.Exchange.ClientAssets

import scala.io.Source

object FileUtils {

  def loadOrders(source: Source) =
    getLineDelemiteredByTab(source)
      .map({
        case Array(clientName,
                   orderType,
                   shareName,
                   pricePerOne,
                   sharesCount) => {
          orderType -> Order(UUID.randomUUID(),
                             clientName,
                             Shares.withName(shareName),
                             pricePerOne.toInt,
                             sharesCount.toInt)
        }
        case _ => throw new Exception("Error loading orders file")
      })
      .foldRight(OrdersStorage(List(), List())) {
        case ((typeMarker, order), orderStorage) => {
          typeMarker match {
            case "s" =>
              orderStorage.copy(sellOrders = order :: orderStorage.sellOrders)
            case "b" =>
              orderStorage.copy(buyOrders = order :: orderStorage.buyOrders)
          }
        }
      }

  def loadClients(source: Source): Exchange.ClientAssets =
    getLineDelemiteredByTab(source)
      .map({
        case Array(name, usdBalance, aBalance, bBalance, cBalance, dBalance) =>
          name -> Assets(usdBalance.toInt,
                         Map(
                           Shares.A -> aBalance.toInt,
                           Shares.B -> bBalance.toInt,
                           Shares.C -> cBalance.toInt,
                           Shares.D -> dBalance.toInt
                         ))
        case _ => throw new Exception("Error loading clients file")
      })
      .toMap

  def getLineDelemiteredByTab(source: Source) =
    source.getLines.map(_.split("\t"))

  def getClientFileContent(clientAssets: ClientAssets) =
    clientAssets.toList
      .sortBy(_._1)
      .map({
        case (clientName, assets) =>
          List(
            clientName,
            assets.usd,
            assets.shares(Shares.A),
            assets.shares(Shares.B),
            assets.shares(Shares.C),
            assets.shares(Shares.D)
          ).mkString("\t")
      })
      .mkString("\n")

  def writeClientsResult(fileName: String, clientAssets: ClientAssets) = {
    val resultText = getClientFileContent(clientAssets)
    new PrintWriter(new File(fileName)) {
      write(resultText)
      close
    }
  }

}
