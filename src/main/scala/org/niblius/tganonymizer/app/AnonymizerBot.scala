package org.niblius.tganonymizer.app

import java.util.concurrent.TimeUnit

import _root_.io.chrisdavenport.log4cats._
import cats._
import cats.data._
import cats.effect.{Concurrent, Sync, Timer}
import cats.implicits._
import fs2._
import org.niblius.tganonymizer.api.dto.Chat
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

  // TODO: logging
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

      // TODO: fix c: Command
      case c: Join           => handleJoin(c.chatId)
      case c: Leave          => handleLeave(c.chatId)
      case c: Message        => handleMessage(c.chatId, c.content)
      case c: SetDelay       => handleSetDelay(c.chatId, c.delay)
      case c: ResetDelay     => handleResetDelay(c.chatId)
      case c: ResetNickname  => handleResetNickname(c.chatId)
      case c: UnknownCommand => handleUnknown(c.chatId)
      case c: ShowAll        => handleShowAll(c.chatId)
      case c: MakeActive     => handleMakeActive(c.chatId, c.target)
    }

    (command match {
      case Join(_) =>
        process
      case _ =>
        persistent
          .getByStatus(true)
          .flatMap(
            _.find(m => m.id == command.chatId)
              .map(_ => process)
              .getOrElse(api.sendMessage(command.chatId, templateNotJoined)))
    }).handleErrorWith(e => logger.error(e.toString))
  }

  private def handleMakeActive(chatId: ChatId, targetStr: String): F[Unit] =
    for {
      chatMember <- inMemory.touchNickname(chatId)
      target     <- persistent.get(targetStr.toLong)
      _ <- target match {
        case Some(t) =>
          for {
            _ <- persistent.update(t.copy(isActive = true))
            _ <- sendEveryone(
              templateMakeActiveSucc(chatMember.name, targetStr))
          } yield ()
        case None =>
          sendEveryone(templateMakeActiveFail(chatMember.name, targetStr))
      }
    } yield ()

  private def templateMakeActiveFail(name: String, target: String): String =
    s"$name tried to add $target to the channel, but such user wasn't found."

  private def templateMakeActiveSucc(name: String, target: String): String =
    s"$name added $target to the channel."

  private def handleShowAll(chatId: ChatId): F[Unit] = {
    def retrieveTelegramChats(
        members: List[ChatMemberSettings]): F[List[Option[(Chat, Boolean)]]] =
      members.traverse(m => api.getChat(m.id).map(_.map(c => (c, m.isActive))))

    for {
      _          <- handleMessage(chatId, showAll)
      allMembers <- persistent.getAll
      chats      <- retrieveTelegramChats(allMembers)
      (active, notActive) = chats.flatten.partition(_._2)
      template            = templateShowAll(active.map(_._1), notActive.map(_._1))
      _ <- sendEveryone(template)
    } yield ()
  }

  private def templateShowAll(active: List[Chat],
                              notActive: List[Chat]): String = {
    def getNameAndId(c: Chat): String = {
      val username = c.first_name
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

  private def handleUnknown(chatId: ChatId): F[Unit] =
    api.sendMessage(chatId, templateUnknown)

  private def templateUnknown: String =
    s"Unknown command. Try $help to list all available commands."

  private def handleResetNickname(chatId: ChatId): F[Unit] =
    inMemory.resetNickname(chatId).void

  private def handleSetDelay(chatId: ChatId, delayStr: String): F[Unit] =
    for {
      _ <- inMemory.touchNickname(chatId)
      _ <- inMemory.setDelay(chatId, Some(delayStr.toInt))
      _ <- api.sendMessage(chatId, templateSetDelay(delayStr))
    } yield ()

  private def templateSetDelay(delay: String): String =
    s"Delay has been set to $delay."

  private def handleResetDelay(chatId: ChatId): F[Unit] =
    for {
      _ <- inMemory.touchNickname(chatId)
      _ <- inMemory.resetDelay(chatId)
      _ <- api.sendMessage(chatId, templateResetDelay)
    } yield ()

  private def templateResetDelay: String =
    s"Delay has been reset."

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
      members <- persistent.getByStatus(true)
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
      active <- persistent.getByStatus(true)
      _ <- active
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
