package org.niblius.tganonymizer.app.domain

import org.niblius.tganonymizer.api.dto.Chat
import org.niblius.tganonymizer.app.domain.BotCommand.{Forward, ForwardOpt}

trait TemplateAlgebra {
  def help: String
  def notJoined: String
  def makeActiveFail(name: String, target: String): String
  def makeActiveSucc(name: String, target: String): String
  def showAll(active: List[Chat], notActive: List[Chat]): String
  def unknown: String
  def resetNickname(name: String): String
  def setDelay(delay: String): String
  def resetDelay: String
  def join(name: String): String
  def leave(name: String): String
  def message(name: String, content: String, from: ForwardOpt): String
  def forward(name: String, from: Forward): String
  def sendItem(name: String): String
  def rejoin(name: String): String
  def alreadyInChannel(name: String): String
}
