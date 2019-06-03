package org.niblius.tganonymizer.app.infrastructure

import org.niblius.tganonymizer.api.dto.Chat
import org.niblius.tganonymizer.app.domain.BotCommand._
import org.niblius.tganonymizer.app.domain.{BotCommand, TemplateAlgebra}

class EnglishTemplate extends TemplateAlgebra {
  val help: String = List(
    s"This bot enables anonymous communication in Telegram. Just type `$joinStr` to enter the chat.",
    s"Other commands:",
    s"`$helpStr` - to show this help message",
    s"`$leaveStr` - to stop receiving messages"
  ).mkString("\n")

  val notJoined = s"You should $joinStr the channel first."

  def makeActiveFail(name: String, target: String): String =
    s"$name tried to add $target to the channel, but such user wasn't found."

  def makeActiveSucc(name: String, target: String): String =
    s"$name added $target to the channel."

  def showAll(active: List[Chat], notActive: List[Chat]): String = {
    def getNameAndId(c: Chat): String = {
      val username = c.firstName
        .orElse(c.title)
        .orElse(c.username)
        .getOrElse("unknown")

      s"$username : ${c.id}"
    }

    val inChatListStr =
      active.map(getNameAndId).mkString("\n")

    val uplural = if (active.size > 1) "s" else ""

    val inChatStr =
      s"There are ${active.size} user$uplural in the channel:\n$inChatListStr"

    val missingListStr = notActive.map(getNameAndId).mkString("\n")

    val bplural = if (notActive.size > 1) "s" else ""

    val missingStr =
      if (notActive.nonEmpty)
        s"and ${notActive.size} bastard$bplural missing:\n $missingListStr"
      else ""

    s"$inChatStr\n$missingStr"
  }

  def resetNickname(name: String): String =
    s"Your new nickname is $name."

  def unknown: String =
    s"Unknown command. Try $helpStr to list all available commands."

  def setDelay(delay: String): String =
    s"Delay has been set to $delay."

  def resetDelay: String =
    s"Delay has been reset."

  def join(name: String): String =
    s"User $name joined the channel."

  def leave(name: String): String =
    s"User $name left the channel."

  def message(name: String, content: String): String =
    s"$name says:\n$content"
}
