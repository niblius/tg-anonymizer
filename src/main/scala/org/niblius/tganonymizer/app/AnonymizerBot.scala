package org.niblius.tganonymizer.app

import java.util.concurrent.TimeUnit

import _root_.io.chrisdavenport.log4cats._
import cats._
import cats.data._
import cats.effect.{Concurrent, Sync, Timer}
import cats.implicits._
import fs2._
import org.niblius.tganonymizer.api.{ChatId, StreamingBotAPI}
import org.niblius.tganonymizer.app.domain.BotCommand._
import org.niblius.tganonymizer.app.domain._

import scala.concurrent.duration.FiniteDuration

/**
  * Starting point of the business logic
  *
  * @param api     telegram bot api
  * @param logger  logger algebra
  */
class AnonymizerBot[F[_]: Timer](
    api: StreamingBotAPI[F],
    logger: Logger[F],
    persistent: PersistentRepositoryAlgebra[F],
    inMemory: InMemoryRepositoryAlgebra[F])(implicit F: Concurrent[F]) {

  /**
    * Launches the bot process
    */
  def launch: Stream[F, Unit] =
    pollCommands.evalMap(handleCommand)

  // TODO: fix nickname everywhere

  private def pollCommands: Stream[F, BotCommand] =
    for {
      update <- api.pollUpdates(0)
      chatIdAndMessage <- Stream.emits(
        update.message.flatMap(a => a.text.map(a.chat.id -> _)).toSeq)
    } yield BotCommand.fromRawMessage(chatIdAndMessage._1, chatIdAndMessage._2)

  private val templateHelp = List(
    s"This bot enables anonymous communication in Telegram. Just type `$join` to enter the chat.",
    s"Other commands:",
    s"`$help` - to show this help message",
    s"`$leave` - to stop receiving messages"
  ).mkString("\n")

  private val templateNotJoined = "You should join the channel first."

  private def handleCommand(command: BotCommand): F[Unit] = {
    println(command)
    def process: F[Unit] = command match {
      case c: ShowHelp =>
        api.sendMessage(c.chatId, templateHelp)

      // TODO: shouldn't do anything if not joined
      // TODO: fix c: Command
      case c: Join          => handleJoin(c.chatId)
      case c: Leave         => handleLeave(c.chatId)
      case c: Message       => handleMessage(c.chatId, c.content)
      case c: SetDelay      => handleSetDelay(c.chatId, c.delay)
      case c: ResetDelay    => handleResetDelay(c.chatId)
      case c: ResetNickname => handleResetNickname(c.chatId)
    }

    (command match {
      case c: Join =>
        process
      case _ =>
        persistent.getActive.flatMap(
          _.find(m => m.id == command.chatId)
            .map(_ => process)
            .getOrElse(api.sendMessage(command.chatId, templateNotJoined)))
    }).handleErrorWith(e => logger.error(e.toString))
  }

  private def handleResetNickname(chatId: ChatId): F[Unit] =
    inMemory.resetNickname(chatId).void

  private def handleSetDelay(chatId: ChatId, delayStr: String): F[Unit] =
    for {
      _ <- inMemory.touchNickname(chatId)
      _ <- inMemory.setDelay(chatId, Some(delayStr.toInt))
      _ <- api.sendMessage(chatId, templateSetDelay(delayStr))
    } yield ()

  private def templateSetDelay(delay: String): String =
    s"Delay has been successfully set to $delay."

  private def handleResetDelay(chatId: ChatId): F[Unit] =
    for {
      _ <- inMemory.touchNickname(chatId)
      _ <- inMemory.resetDelay(chatId)
      _ <- api.sendMessage(chatId, templateResetDelay)
    } yield ()

  private def templateResetDelay: String =
    s"Delay has been successfully reset."

  private def handleJoin(chatId: ChatId): F[Unit] =
    for {
      member <- persistent.get(chatId)
      _ <- member match {
        case Some(cm @ ChatMemberSettings(_, _, false)) =>
          persistent.update(cm.copy(isActive = true)).void
        case None =>
          persistent
            .create(ChatMemberSettings(chatId, "x", isActive = true))
            .void
        case _ =>
          F.pure(())
      }
      nickname <- inMemory.touchNickname(chatId)
      template = joinTemplate(nickname.name)
      _ <- sendEveryone(template)
    } yield ()

  private def sendEveryone(content: String,
                           but: Option[ChatId] = None): F[Unit] =
    for {
      members <- persistent.getActive
      target = but.map(id => members.filter(_.id != id)).getOrElse(members)
      _ <- target.traverse(m => api.sendMessage(m.id, content))
    } yield ()

  private def joinTemplate(name: String): String =
    s"User $name joined the channel."

  private def handleLeave(chatId: ChatId): F[Unit] =
    OptionT(persistent.get(chatId))
      .semiflatMap(settings =>
        for {
          cm <- inMemory.touchNickname(chatId)
          template = templateLeave(cm.name)
          _ <- sendEveryone(template)
          _ <- persistent.update(settings.copy(isActive = false))
        } yield ())
      .value
      .void

  private def templateLeave(name: String): String =
    s"User $name left the channel."

  private def handleMessage(chatId: ChatId, content: String): F[Unit] =
    for {
      nickname <- inMemory.touchNickname(chatId)
      template = messageTemplate(nickname.name, content)
      activeMembers <- persistent.getActive
      _ <- activeMembers
        .filter(m => m.id != chatId)
        .traverse { m =>
          nickname.delay
            .map(delay => sendDelayedMessage(m.id, template, delay))
            .getOrElse(api.sendMessage(m.id, template))
        }
    } yield ()

  private def sendDelayedMessage(chatId: ChatId,
                                 content: String,
                                 delay: SECONDS): F[Unit] = {
    val send = for {
      _ <- logger.info("sent delayed message")
      _ <- Timer[F].sleep(FiniteDuration.apply(delay, TimeUnit.SECONDS))
      _ <- api.sendMessage(chatId, content)
    } yield ()
    F.start(send).void
  }

  private def messageTemplate(name: String, content: String): String =
    s"$name says:\n$content"
}

object AnonymizerBot {
  def apply[F[_]: Timer: Concurrent](
      api: StreamingBotAPI[F],
      logger: Logger[F],
      members: PersistentRepositoryAlgebra[F],
      nicknameStore: InMemoryRepositoryAlgebra[F]): AnonymizerBot[F] =
    new AnonymizerBot(api, logger, members, nicknameStore)
}
