package org.niblius.tganonymizer.app

import _root_.io.chrisdavenport.log4cats._
import cats.effect.Sync
import cats.implicits._
import fs2._
import org.niblius.tganonymizer.api.StreamingBotAPI
import org.niblius.tganonymizer.app.domain.BotCommand

/**
  * Todo list telegram bot
  * When launched, polls incoming commands and processes them using todo-list storage algebra.
  *
  * @param api     telegram bot api
  * @param storage storage algebra for todo-list items
  * @param logger  logger algebra
  */
class AnonymizerBot[F[_]](api: StreamingBotAPI[F], logger: Logger[F])(
    implicit F: Sync[F]) {

  /**
    * Launches the bot process
    */
  def launch: Stream[F, Unit] =
    pollCommands.evalMap(handleCommand)

  private def pollCommands: Stream[F, BotCommand] =
    for {
      update <- api.pollUpdates(0)
      chatIdAndMessage <- Stream.emits(
        update.message.flatMap(a => a.text.map(a.chat.id -> _)).toSeq)
    } yield BotCommand.fromRawMessage(chatIdAndMessage._1, chatIdAndMessage._2)

  private def handleCommand(command: BotCommand): F[Unit] =
    logger.info("got a command")
  /* command match {
    case c: ClearTodoList => clearTodoList(c.chatId)
    case c: ShowTodoList  => showTodoList(c.chatId)
    case c: AddEntry      => addItem(c.chatId, c.content)
    case c: ShowHelp =>
      api.sendMessage(
        c.chatId,
        List(
          "This bot stores your todo-list. Just write a task and the bot will store it! Other commands:",
          s"`$help` - show this help message",
          s"`$show` - show current todo-list",
          s"`$clear` - clear current list (vacation!)",
        ).mkString("\n")
      )
  }

 */
}
