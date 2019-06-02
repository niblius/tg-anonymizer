package org.niblius.tganonymizer.app.domain

import org.niblius.tganonymizer.api.ChatId

sealed trait BotCommand {
  def chatId: ChatId
}

object BotCommand {

  case class ShowHelp(chatId: ChatId)                 extends BotCommand
  case class Join(chatId: ChatId)                     extends BotCommand
  case class Leave(chatId: ChatId)                    extends BotCommand
  case class Message(chatId: ChatId, content: String) extends BotCommand
  case class SetDelay(chatId: ChatId, delay: String)  extends BotCommand
  case class ResetNickname(chatId: ChatId)            extends BotCommand
  case class ResetDelay(chatId: ChatId)               extends BotCommand

  def fromRawMessage(chatId: ChatId, message: String): BotCommand =
    message match {
      case `help` | "/start" => ShowHelp(chatId)
      case `join`            => Join(chatId)
      case `leave`           => Leave(chatId)
      case setDelay(delay)   => SetDelay(chatId, delay)
      case `resetDelay`      => ResetDelay(chatId)
      case `resetNickname`   => ResetNickname(chatId: ChatId)
      // TODO: if starts with '/' - unknown command
      case _ => Message(chatId, message)
    }

  val help          = "/help"
  val join          = "/join"
  val leave         = "/leave"
  val setDelay      = "\\/set_delay ([0-9]{1,4})".r
  val resetDelay    = "/reset_delay"
  val resetNickname = "/reset_nickname"

  // TODO: showAllActive
  // TODO: ban
}
