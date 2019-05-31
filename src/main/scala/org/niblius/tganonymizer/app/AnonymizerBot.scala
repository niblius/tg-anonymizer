package org.niblius.tganonymizer.app

import _root_.io.chrisdavenport.log4cats._
import cats.data.OptionT
import cats.effect.Sync
import cats.implicits._
import fs2._
import org.niblius.tganonymizer.api.{ChatId, StreamingBotAPI}
import org.niblius.tganonymizer.app.domain.BotCommand._
import org.niblius.tganonymizer.app.domain.{
  BotCommand,
  ChatMember,
  MemberRepositoryAlgebra
}

/**
  * Starting point of the business logic
  *
  * @param api     telegram bot api
  * @param logger  logger algebra
  */
class AnonymizerBot[F[_]](
    api: StreamingBotAPI[F],
    logger: Logger[F],
    members: MemberRepositoryAlgebra[F])(implicit F: Sync[F]) {

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
    (command match {
      case c: ShowHelp =>
        api.sendMessage(
          c.chatId,
          List(
            s"This bot enables anonymous communication in Telegram. Just type `$join` to enter the chat.",
            s"Other commands:",
            s"`$help` - to show this help message",
            s"`$leave` - to stop receiving messages",
          ).mkString("\n")
        )
      case c: Join    => handleJoin(c.chatId)
      case c: Leave   => handleLeave(c.chatId)
      case c: Message => handleMessage(c.chatId, c.content)
      case _          => Sync[F].pure(())
    }).handleErrorWith(e => logger.error(e.toString))

  private def handleJoin(chatId: ChatId): F[Unit] =
    members
      .get(chatId)
      .flatMap {
        case Some(cm @ ChatMember(_, _, false)) =>
          members.update(cm.copy(isActive = true)).void
        case None =>
          members.create(ChatMember(chatId, "x", isActive = true)).void
        case _ =>
          F.pure(())
      }

  private def handleLeave(chatId: ChatId): F[Unit] =
    OptionT(members.get(chatId))
      .semiflatMap(cm => members.update(cm.copy(isActive = false)))
      .value
      .void

  private def handleMessage(chatId: ChatId, text: String): F[Unit] =
    members.getActive
      .flatMap(_.filter(m => m.id != chatId).traverse(m =>
        api.sendMessage(m.id, text)))
      .void
}

object AnonymizerBot {
  def apply[F[_]](api: StreamingBotAPI[F],
                  logger: Logger[F],
                  members: MemberRepositoryAlgebra[F])(
      implicit F: Sync[F]): AnonymizerBot[F] =
    new AnonymizerBot(api, logger, members)
}
