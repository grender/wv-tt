package com.github.grender.wv_tt

import java.io.{File, PrintWriter}

import scala.io.Source

object FileUtils {

  def getClientFileContent(clients: Iterable[Client]) = clients.map(_.forFile).mkString("\n")

  def writeClientsResult(fileName:String,clients: Iterable[Client]) = {
    val resultText = getClientFileContent(clients)
    new PrintWriter(new File(fileName)) {
      write(resultText)
      close
    }
  }

  def getLineDelemiteredByTab(source: Source) = source.getLines.map(_.split("\t"))

  def loadOrders(source: Source) = getLineDelemiteredByTab(source).map({
    case Array(clientName, orderType, shareName, pricePerOne, sharesCount) => {
      new Order(clientName, OrderType.fromLetter(orderType), SharesName.fromLetter(shareName), pricePerOne.toInt, sharesCount.toInt)
    }
    case _ => throw new Exception("Error loading orders file")
  })

  def loadClients(source: Source) = getLineDelemiteredByTab(source).map({
    case Array(name, usdBalance, aBalance, bBalance, cBalance, dBalance) => new Client(name, usdBalance.toInt, aBalance.toInt, bBalance.toInt, cBalance.toInt, dBalance.toInt)
    case _ => throw new Exception("Error loading clients file")
  }).toIterable

}
