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
import org.niblius.tganonymizer.app.infrastructure.EnglishTemplate

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
    userRepo: UserRepositoryAlgebra[F],
    memberRepo: ChatMemberServiceAlgebra[F])(implicit F: Concurrent[F]) {

  private val language = new EnglishTemplate

  /**
    * Launches the bot process
    */
  def launch: Stream[F, Unit] =
    pollCommands.evalMap(handleCommand)

  // TODO: logging

  private def pollCommands: Stream[F, BotCommand] =
    for {
      update <- api.pollUpdates(0)
      chatIdAndMessage <- Stream.emits(
        update.message.flatMap(a => a.text.map(a.chat.id -> _)).toSeq)
    } yield BotCommand.fromRawMessage(chatIdAndMessage._1, chatIdAndMessage._2)

  private def handleCommand(command: BotCommand): F[Unit] = {
    def process: F[Unit] = command match {
      case c: ShowHelp       => handleHelp(c.chatId)
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
        userRepo
          .getByIsActive(true)
          .flatMap(
            _.find(m => m.chatId == command.chatId)
              .map(_ => process)
              .getOrElse(api.sendMessage(command.chatId, language.notJoined)))
    }).handleErrorWith(e => logger.error(e.toString))
  }

  private def handleHelp(chatId: ChatId): F[Unit] =
    api.sendMessage(chatId, language.help)

  private def handleMakeActive(chatId: ChatId, targetIdStr: String): F[Unit] =
    for {
      member     <- memberRepo.touch(chatId)
      targetUser <- userRepo.get(targetIdStr.toLong)
      _ <- targetUser match {
        case Some(t) =>
          for {
            _ <- userRepo.update(t.copy(isActive = true))
            _ <- sendEveryone(language.makeActiveSucc(member.name, targetIdStr))
          } yield ()
        case None =>
          sendEveryone(language.makeActiveFail(member.name, targetIdStr))
      }
    } yield ()

  private def handleShowAll(chatId: ChatId): F[Unit] = {
    def retrieveTelegramChats(
        users: List[User]): F[List[Option[(Chat, Boolean)]]] =
      users.traverse(usr =>
        api.getChat(usr.chatId).map(_.map(chat => (chat, usr.isActive))))

    for {
      _                <- handleMessage(chatId, showAllStr)
      allUsers         <- userRepo.getAll
      chatsAndIsActive <- retrieveTelegramChats(allUsers)
      (active, notActive) = chatsAndIsActive.flatten.partition(_._2)
      template            = language.showAll(active.map(_._1), notActive.map(_._1))
      _ <- sendEveryone(template)
    } yield ()
  }

  private def handleUnknown(chatId: ChatId): F[Unit] =
    api.sendMessage(chatId, language.unknown)

  private def handleResetNickname(chatId: ChatId): F[Unit] =
    for {
      member <- memberRepo.resetName(chatId)
      _      <- api.sendMessage(chatId, language.resetNickname(member.name))
    } yield ()

  private def handleSetDelay(chatId: ChatId, delayStr: String): F[Unit] =
    for {
      _ <- memberRepo.touch(chatId)
      _ <- memberRepo.setDelay(chatId, Some(delayStr.toInt))
      _ <- api.sendMessage(chatId, language.setDelay(delayStr))
    } yield ()

  private def handleResetDelay(chatId: ChatId): F[Unit] =
    for {
      _ <- memberRepo.touch(chatId)
      _ <- memberRepo.resetDelay(chatId)
      _ <- api.sendMessage(chatId, language.resetDelay)
    } yield ()

  private def handleJoin(chatId: ChatId): F[Unit] =
    for {
      user <- userRepo.get(chatId)
      _ <- user match {
        case Some(cm @ User(_, false)) =>
          userRepo.update(cm.copy(isActive = true)).void
        case None =>
          userRepo
            .create(User(chatId, isActive = true))
            .void
        case _ =>
          F.pure(())
      }
      member <- memberRepo.touch(chatId)
      template = language.join(member.name)
      _ <- sendEveryone(template)
    } yield ()

  private def sendEveryone(content: String,
                           but: Option[ChatId] = None): F[Unit] =
    for {
      users <- userRepo.getByIsActive(true)
      target = but.map(id => users.filter(_.chatId != id)).getOrElse(users)
      _ <- target.traverse(usr => api.sendMessage(usr.chatId, content))
    } yield ()

  private def handleLeave(chatId: ChatId): F[Unit] =
    OptionT(userRepo.get(chatId))
      .semiflatMap(settings =>
        for {
          member <- memberRepo.touch(chatId)
          template = language.leave(member.name)
          _ <- sendEveryone(template)
          _ <- userRepo.update(settings.copy(isActive = false))
        } yield ())
      .value
      .void

  private def handleMessage(chatId: ChatId, content: String): F[Unit] =
    for {
      member <- memberRepo.touch(chatId)
      template = language.message(member.name, content)
      active <- userRepo.getByIsActive(true)
      _ <- active
        .filter(usr => usr.chatId != chatId)
        .traverse { usr =>
          member.delay
            .map(delay => sendDelayedMessage(usr.chatId, template, delay))
            .getOrElse(api.sendMessage(usr.chatId, template))
        }
    } yield ()

  private def sendDelayedMessage(chatId: ChatId,
                                 content: String,
                                 delay: SECOND): F[Unit] = {
    val send = for {
      _ <- Timer[F].sleep(FiniteDuration.apply(delay, TimeUnit.SECONDS))
      _ <- api.sendMessage(chatId, content)
    } yield ()
    F.start(send).void
  }
}

object AnonymizerBot {
  def apply[F[_]: Timer: Concurrent](
      api: StreamingBotAPI[F],
      logger: Logger[F],
      userRepo: UserRepositoryAlgebra[F],
      memberRepo: ChatMemberServiceAlgebra[F]): AnonymizerBot[F] =
    new AnonymizerBot(api, logger, userRepo, memberRepo)
}
