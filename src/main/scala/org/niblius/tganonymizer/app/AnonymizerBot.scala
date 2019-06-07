package org.niblius.tganonymizer.app

import java.util.concurrent.TimeUnit

import _root_.io.chrisdavenport.log4cats._
import cats._
import cats.data._
import cats.effect.{Concurrent, Fiber, Sync, Timer}
import cats.implicits._
import fs2._
import org.niblius.tganonymizer.api.dto.{
  ApiError,
  Chat,
  InputMediaPhoto,
  Message => ApiMessage
}
import org.niblius.tganonymizer.api.{ChatId, FileId, MessageId, StreamingBotAPI}
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
    memberRepo: ChatMemberServiceAlgebra[F],
    messageRepo: MessageRepositoryAlgebra[F])(implicit F: Concurrent[F]) {

  private val language = new EnglishTemplate

  /**
    * Launches the bot process
    */
  def launch: Stream[F, Unit] =
    pollCommands.evalMap(handleCommand)

  // TODO: pinned - how?
  // TODO: reply
  // TODO: editing
  // TODO: deleting - reply to message with /delete
  // TODO: periodically run clean up process to remove non-existing chats and those that banned bot
  // TODO: delay for all messages, not only plain

  private def pollCommands: Stream[F, BotCommand] =
    for {
      update  <- api.pollUpdates(0)
      _       <- Stream.eval(logger.debug(s"Received an update:\n${update.toString}"))
      message <- Stream.emits(update.message.toSeq)
      cmdOpt = BotCommand.fromRawMessage(message)
      _ <- cmdOpt
        .map(cmd => Stream.eval(logger.debug(cmd.toString)))
        .getOrElse(Stream.eval(logger.info("Received unknown message.")))
      command <- Stream.emits(cmdOpt.toSeq)
    } yield command

  private def handleCommand(command: BotCommand): F[Unit] = {
    val process: F[Unit] = command match {
      case c: ShowHelp       => handleHelp(c.chatId, c.messageId)
      case c: Join           => handleJoin(c.chatId)
      case c: Leave          => handleLeave(c.chatId)
      case c: SetDelay       => handleSetDelay(c.chatId, c.delay)
      case c: ResetDelay     => handleResetDelay(c.chatId)
      case c: ResetNickname  => handleResetNickname(c.chatId)
      case c: UnknownCommand => handleUnknown(c.chatId)
      case c: ShowAll        => handleShowAll(c.chatId, c.messageId)
      case c: MakeActive =>
        handleMakeActive(c.chatId, c.target)
      case c: PlainMessage =>
        handleMessage(c.chatId, c.messageId, c.content, c.replyId, c.from)

      case c: SendMediaGroup =>
        trivia(c.chatId, c.messageId, c.from)(handleMediaGroup(c.fileIds))
      case c: SendLocation =>
        trivia(c.chatId, c.messageId, c.from)(
          handleLocation(c.longitude, c.latitude))
      case c: SendPhoto =>
        trivia(c.chatId, c.messageId, c.from)(handlePhoto(c.fileId))
      case c: SendAudio =>
        trivia(c.chatId, c.messageId, c.from)(handleAudio(c.fileId))
      case c: SendDocument =>
        trivia(c.chatId, c.messageId, c.from)(handleDocument(c.fileId))
      case c: SendAnimation =>
        trivia(c.chatId, c.messageId, c.from)(handleAnimation(c.fileId))
      case c: SendSticker =>
        trivia(c.chatId, c.messageId, c.from)(handleSticker(c.fileId))
      case c: SendVideo =>
        trivia(c.chatId, c.messageId, c.from)(handleVideo(c.fileId))
      case c: SendVoice =>
        trivia(c.chatId, c.messageId, c.from)(handleVoice(c.fileId))
      case c: SendVideoNote =>
        trivia(c.chatId, c.messageId, c.from)(handleVideoNote(c.fileId))
    }

    (command match {
      case Join(_) =>
        process
      case _ =>
        userRepo.get(command.chatId).flatMap {
          case Some(user) if user.isActive => process
          case _ =>
            val askToJoin =
              api.sendMessage(command.chatId, language.notJoined).void
            for {
              apiUser <- api.getChat(command.chatId)
              privateApiUser = apiUser.toOption.filter(_.`type` == "private")
              _ <- privateApiUser.map(_ => askToJoin).getOrElse(F.pure(()))
            } yield ()
        }
    }).handleErrorWith(e => logger.error(e)("Error during command processing"))
  }

  /**
    * Wrapper that performs routine operations:
    *  - checks if it's a forward
    *  - updates the nickname timestamp
    * @param chatId
    * @param from
    * @return
    */
  def trivia(chatId: ChatId, messageId: MessageId, from: ForwardOpt)(
      sendItem: (ChatId, MessageId) => F[Unit]): F[Unit] = {
    for {
      member <- memberRepo.touch(chatId)
      content = from
        .map(forw => language.forward(member.name, forw))
        .getOrElse(language.sendItem(member.name))
      _ <- sendEveryone(content, None, chatId.some) // TODO: can't reply to the title
      _ <- sendItem(chatId, messageId)
    } yield ()
  }

  private def makeSelfSource(chatId: ChatId, messageId: MessageId): Message = {
    val _msg = Message(chatId, messageId)
    _msg.copy(source = _msg.some)
  }

  private def sendEveryoneButSelfAndRecord(
      chatId: ChatId,
      messageId: MessageId,
      sendItem: User => F[Either[ApiError, ApiMessage]]): F[Unit] = {
    val source = makeSelfSource(chatId, messageId)
    for {
      _ <- messageRepo.create(source)
      _ <- execForAll(u => recordMessage(sendItem(u), source.some).void,
                      chatId.some)
    } yield ()
  }

  def handleMediaGroup(fileIds: List[FileId])(chatId: ChatId,
                                              messageId: MessageId): F[Unit] = {
    val media = fileIds.map(id => InputMediaPhoto("photo", id))
    for {
      _ <- logger.info(
        s"User $chatId sends a media group of ${fileIds.size} items")
      sendItem = (usr: User) => api.sendMediaGroup(usr.chatId, media)
      _ <- sendEveryoneButSelfAndRecord(chatId, messageId, sendItem)
    } yield ()
  }

  def handleLocation(longitude: Float, latitude: Float)(
      chatId: ChatId,
      messageId: MessageId): F[Unit] =
    for {
      _ <- logger.info(s"User $chatId sends a location")
      sendItem = (usr: User) =>
        api.sendLocation(usr.chatId, latitude, longitude)
      _ <- sendEveryoneButSelfAndRecord(chatId, messageId, sendItem)
    } yield ()

  def handlePhoto(fileId: FileId)(chatId: ChatId,
                                  messageId: MessageId): F[Unit] =
    for {
      _ <- logger.info(s"User $chatId sends a photo")
      sendItem = (usr: User) => api.sendPhoto(usr.chatId, fileId)
      _ <- sendEveryoneButSelfAndRecord(chatId, messageId, sendItem)
    } yield ()

  def handleAudio(fileId: FileId)(chatId: ChatId,
                                  messageId: MessageId): F[Unit] =
    for {
      _ <- logger.info(s"User $chatId sends an audio")
      sendItem = (usr: User) => api.sendAudio(usr.chatId, fileId)
      _ <- sendEveryoneButSelfAndRecord(chatId, messageId, sendItem)
    } yield ()

  def handleDocument(fileId: FileId)(chatId: ChatId,
                                     messageId: MessageId): F[Unit] =
    for {
      _ <- logger.info(s"User $chatId sends a document")
      sendItem = (usr: User) => api.sendDocument(usr.chatId, fileId)
      _ <- sendEveryoneButSelfAndRecord(chatId, messageId, sendItem)
    } yield ()

  def handleAnimation(fileId: FileId)(chatId: ChatId,
                                      messageId: MessageId): F[Unit] =
    for {
      _ <- logger.info(s"User $chatId sends an animation")
      sendItem = (usr: User) => api.sendAnimation(usr.chatId, fileId)
      _ <- sendEveryoneButSelfAndRecord(chatId, messageId, sendItem)
    } yield ()

  def handleSticker(fileId: FileId)(chatId: ChatId,
                                    messageId: MessageId): F[Unit] =
    for {
      _ <- logger.info(s"User $chatId sends a sticker")
      sendItem = (usr: User) => api.sendSticker(usr.chatId, fileId)
      _ <- sendEveryoneButSelfAndRecord(chatId, messageId, sendItem)
    } yield ()

  def handleVideo(fileId: FileId)(chatId: ChatId,
                                  messageId: MessageId): F[Unit] =
    for {
      _ <- logger.info(s"User $chatId sends a video")
      sendItem = (usr: User) => api.sendVideo(usr.chatId, fileId)
      _ <- sendEveryoneButSelfAndRecord(chatId, messageId, sendItem)
    } yield ()

  def handleVoice(fileId: FileId)(chatId: ChatId,
                                  messageId: MessageId): F[Unit] =
    for {
      _ <- logger.info(s"User $chatId sends a voice")
      sendItem = (usr: User) => api.sendVoice(usr.chatId, fileId)
      _ <- sendEveryoneButSelfAndRecord(chatId, messageId, sendItem)
    } yield ()

  def handleVideoNote(fileId: FileId)(chatId: ChatId,
                                      messageId: MessageId): F[Unit] =
    for {
      _ <- logger.info(s"User $chatId sends a video note")
      sendItem = (usr: User) => api.sendVideoNote(usr.chatId, fileId)
      _ <- sendEveryoneButSelfAndRecord(chatId, messageId, sendItem)
    } yield ()

  private def handleHelp(chatId: ChatId, messageId: MessageId): F[Unit] =
    for {
      _ <- logger.info(s"User $chatId requests help")
      _ <- handleMessage(chatId, messageId, helpStr, None, None)
      _ <- sendEveryoneMakeSrc(chatId, language.help)
    } yield ()

  private def handleMakeActive(chatId: ChatId, targetIdStr: String): F[Unit] =
    for {
      _          <- logger.info(s"User $chatId makes active user $targetIdStr")
      member     <- memberRepo.touch(chatId)
      targetUser <- userRepo.get(targetIdStr.toLong)
      _ <- targetUser match {
        case Some(t) =>
          val template = language.makeActiveSucc(member.name, targetIdStr)
          for {
            _ <- userRepo.update(t.copy(isActive = true))
            _ <- sendEveryoneMakeSrc(chatId, template)
          } yield ()
        case None =>
          val template = language.makeActiveFail(member.name, targetIdStr)
          sendEveryoneMakeSrc(chatId, template)
      }
    } yield ()

  private def handleShowAll(chatId: ChatId, messageId: MessageId): F[Unit] = {
    def retrieveTelegramChats(
        users: List[User]): F[List[Option[(Chat, Boolean)]]] =
      users.traverse(
        usr =>
          api
            .getChat(usr.chatId)
            .map(either => either.map(chat => (chat, usr.isActive)).toOption))

    for {
      _                <- logger.info(s"User $chatId requests show all ($showAllStr)")
      _                <- handleMessage(chatId, messageId, showAllStr, None, None)
      allUsers         <- userRepo.getAll
      chatsAndIsActive <- retrieveTelegramChats(allUsers)
      (active, notActive) = chatsAndIsActive.flatten.partition(_._2)
      template            = language.showAll(active.map(_._1), notActive.map(_._1))
      _ <- sendEveryoneMakeSrc(chatId, template)
    } yield ()
  }

  private def handleUnknown(chatId: ChatId): F[Unit] =
    for {
      _ <- logger.info(s"Received unknown command from $chatId")
      _ <- sendMessage(chatId, None, None, language.unknown)
    } yield ()

  private def handleResetNickname(chatId: ChatId): F[Unit] =
    for {
      _      <- logger.info(s"User $chatId resets nickname")
      member <- memberRepo.resetName(chatId)
      _      <- sendMessage(chatId, None, None, language.resetNickname(member.name))
    } yield ()

  private def handleSetDelay(chatId: ChatId, delayStr: String): F[Unit] =
    for {
      _ <- logger.info(s"User $chatId sets delay to $delayStr")
      _ <- memberRepo.touch(chatId)
      _ <- memberRepo.setDelay(chatId, Some(delayStr.toInt))
      _ <- sendMessage(chatId, None, None, language.setDelay(delayStr))
    } yield ()

  private def handleResetDelay(chatId: ChatId): F[Unit] =
    for {
      _ <- logger.info(s"User $chatId resets nickname")
      _ <- memberRepo.touch(chatId)
      _ <- memberRepo.resetDelay(chatId)
      _ <- sendMessage(chatId, None, None, language.resetDelay)
    } yield ()

  private def handleJoin(chatId: ChatId): F[Unit] =
    for {
      _      <- logger.info(s"User $chatId tries to join the channel")
      user   <- userRepo.get(chatId)
      member <- memberRepo.touch(chatId)
      template <- user match {
        case Some(cm @ User(_, false)) =>
          userRepo
            .update(cm.copy(isActive = true))
            .as(language.rejoin(member.name))
        case None =>
          userRepo
            .create(User(chatId, isActive = true))
            .as(language.join(member.name))
        case _ =>
          F.pure(language.alreadyInChannel(member.name))
      }
      _ <- sendEveryoneMakeSrc(chatId, template)
    } yield ()

  /*
   * Same as sendEveryone, but makes one of the sent messages a source
   */
  private def sendEveryoneMakeSrc(chatId: ChatId, template: String): F[Unit] =
    for {
      source <- sendMessageMakeSrc(chatId, None, template)
      _      <- sendEveryone(template, source, chatId.some)
    } yield ()

  private def sendEveryone(content: String,
                           source: Option[Message],
                           but: Option[ChatId] = None): F[Unit] =
    execForAll(usr => sendMessage(usr.chatId, None, source, content).void, but)

  private def execForAll(action: User => F[Unit],
                         but: Option[ChatId] = None): F[Unit] =
    for {
      users <- userRepo.getByIsActive(true)
      target = but.map(id => users.filter(_.chatId != id)).getOrElse(users)
      _ <- target.traverse(action)
    } yield ()

  private def handleLeave(chatId: ChatId): F[Unit] =
    for {
      _       <- logger.info(s"User $chatId tries to leave the channel")
      userOpt <- userRepo.get(chatId)
      leaveActionOpt = userOpt.map(user =>
        for {
          member <- memberRepo.touch(chatId)
          template = language.leave(member.name)
          _ <- sendEveryoneMakeSrc(chatId, template)
          _ <- userRepo.update(user.copy(isActive = false))
        } yield ())
      _ <- leaveActionOpt.getOrElse(F.pure(()))
    } yield ()

  /**
    * Each user get different message from the bot. To match replies correctly
    * we need to get the chat-message correspondence map.
    */
  // TODO: rename?
  private def getChatToMessageMap(
      chatId: ChatId,
      replyIdOpt: Option[MessageId]): F[Map[ChatId, MessageId]] =
    for {
      replyOpt <- replyIdOpt
        .map(replyId => messageRepo.get(chatId, replyId))
        .getOrElse(F.pure(None))
      messages <- replyOpt
        .flatMap(_.source)
        .map(source => messageRepo.getBySource(source))
        .getOrElse(F.pure(List[Message]()))
    } yield messages.groupBy(_.chatId).mapValues(_.head.messageId)

  private def handleMessage(chatId: ChatId,
                            messageId: ChatId,
                            content: String,
                            replyId: Option[MessageId],
                            from: ForwardOpt): F[Unit] = {

    def send(delayOpt: Option[SECOND],
             replies: Map[ChatId, MessageId],
             source: Option[Message],
             template: String)(usr: User): F[Unit] = {
      val sendNow =
        sendMessage(usr.chatId, replies.get(usr.chatId), source, template).void

      val sendDelayed = execDelayed(sendNow)(_)

      if (usr.chatId != chatId)
        delayOpt.map(sendDelayed).map(_.void).getOrElse(sendNow)
      else F.pure(())
    }

    for {
      _ <- logger.info(
        s"User $chatId sends message, forward from: $from, reply on: $replyId")
      member <- memberRepo.touch(chatId)
      active <- userRepo.getByIsActive(true)
      template = language.message(member.name, content, from)
      source   = makeSelfSource(chatId, messageId)
      _       <- messageRepo.create(source)
      replies <- getChatToMessageMap(chatId, replyId)
      _       <- active.traverse(send(member.delay, replies, source.some, template))
    } yield ()
  }

  /*
   * Same as sendMessage, but links to itself as a source
   */
  private def sendMessageMakeSrc(chatId: ChatId,
                                 replyId: Option[MessageId],
                                 content: String): F[Option[Message]] =
    (for {
      apiMsg <- EitherT(api.sendMessage(chatId, content, replyId)).toOption
      source = makeSelfSource(chatId, apiMsg.messageId)
      _ <- OptionT.liftF(messageRepo.create(source))
    } yield source).value

  def recordMessage(sendItem: F[Either[ApiError, ApiMessage]],
                    source: Option[Message]): F[Option[Message]] =
    recordMessageOpt(sendItem.map(_.toOption), source)

  def recordMessageOpt(sendItem: F[Option[ApiMessage]],
                       source: Option[Message]): F[Option[Message]] =
    (for {
      apiMsg <- OptionT(sendItem)
      msg = Message(apiMsg.chat.id, apiMsg.messageId, source)
      _ <- OptionT.liftF(messageRepo.create(msg))
    } yield msg).value

  private def sendMessage(chatId: ChatId,
                          replyId: Option[MessageId],
                          source: Option[Message],
                          content: String): F[Option[Message]] =
    recordMessage(api.sendMessage(chatId, content, replyId), source)

  private def execDelayed[A](action: F[A])(delay: SECOND): F[Fiber[F, A]] = {
    val fiber = for {
      _   <- Timer[F].sleep(FiniteDuration.apply(delay, TimeUnit.SECONDS))
      _   <- logger.info(s"Executing delayed action after $delay seconds)")
      res <- action
    } yield res

    F.start(fiber)
  }
}

object AnonymizerBot {
  def apply[F[_]: Timer: Concurrent](
      api: StreamingBotAPI[F],
      logger: Logger[F],
      userRepo: UserRepositoryAlgebra[F],
      memberRepo: ChatMemberServiceAlgebra[F],
      messageRepo: MessageRepositoryAlgebra[F]): AnonymizerBot[F] =
    new AnonymizerBot(api, logger, userRepo, memberRepo, messageRepo)
}
