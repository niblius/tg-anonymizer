package org.niblius.tganonymizer.app.domain

import org.niblius.tganonymizer.api.ChatId

sealed trait BotCommand

object BotCommand {

  case class ShowHelp(chatId: ChatId)                 extends BotCommand
  case class Join(chatId: ChatId)                     extends BotCommand
  case class Leave(chatId: ChatId)                    extends BotCommand
  case class Message(chatId: ChatId, content: String) extends BotCommand

  def fromRawMessage(chatId: ChatId, message: String): BotCommand =
    message match {
      case `help` | "/start" => ShowHelp(chatId)
      case `join`            => Join(chatId)
      case `leave`           => Leave(chatId)
      case _                 => Message(chatId, message)
    }

  val help  = "/help"
  val join  = "/join"
  val leave = "/leave"
  // val sendIn
  // val begin
  // val end
  // val discard
  // val randomizeTimer
}
