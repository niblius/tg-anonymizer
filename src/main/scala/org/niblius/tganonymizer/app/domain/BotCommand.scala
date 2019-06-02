package org.niblius.tganonymizer.app.domain

import org.niblius.tganonymizer.api.ChatId

sealed trait BotCommand {
  def chatId: ChatId
}

object BotCommand {

  case class ShowHelp(chatId: ChatId)                   extends BotCommand
  case class Join(chatId: ChatId)                       extends BotCommand
  case class Leave(chatId: ChatId)                      extends BotCommand
  case class Message(chatId: ChatId, content: String)   extends BotCommand
  case class SetDelay(chatId: ChatId, delay: String)    extends BotCommand
  case class ResetNickname(chatId: ChatId)              extends BotCommand
  case class ResetDelay(chatId: ChatId)                 extends BotCommand
  case class UnknownCommand(chatId: ChatId)             extends BotCommand
  case class ShowAll(chatId: ChatId)                    extends BotCommand
  case class MakeActive(chatId: ChatId, target: String) extends BotCommand

  def fromRawMessage(chatId: ChatId, message: String): BotCommand =
    message match {
      case `helpStr` | "/start" => ShowHelp(chatId)
      case `joinStr`            => Join(chatId)
      case `leaveStr`           => Leave(chatId)
      case setDelay(delay)      => SetDelay(chatId, delay)
      case `resetDelayStr`      => ResetDelay(chatId)
      case `resetNicknameStr`   => ResetNickname(chatId)
      case `showAllStr`         => ShowAll(chatId: ChatId)
      case makeActive(target)   => MakeActive(chatId, target)
      case unknownCommand()     => UnknownCommand(chatId)
      case _                    => Message(chatId, message)
    }

  val helpStr          = "/help"
  val joinStr          = "/join"
  val leaveStr         = "/leave"
  val setDelay         = "\\/set_delay ([0-9]{1,4})".r
  val setDelayStr      = "/set_delay DELAY"
  val resetDelayStr    = "/reset_delay"
  val resetNicknameStr = "/reset_nickname"
  val unknownCommand   = "\\/.*".r
  val makeActiveStr    = "/add ID"
  val showAllStr       = "/members"
  val makeActive       = "/add ([0-9]{1,19})".r

  // TODO: ban
  // TODO: assignName
}
