package org.niblius.tganonymizer.app.domain

import cats.implicits._
import cats._
import cats.data.NonEmptyList
import cats.syntax._

import scala.collection.immutable.Stream
import org.niblius.tganonymizer.api
import org.niblius.tganonymizer.api.dto.{
  PhotoSize,
  User => apiUser,
  Chat => apiChat
}
import org.niblius.tganonymizer.api.{ChatId, FileId, MessageId}

sealed trait BotCommand {
  def chatId: ChatId
}

object BotCommand {
  type Forward    = Either[apiUser, apiChat]
  type ForwardOpt = Option[Forward]

  case class ShowHelp(chatId: ChatId, messageId: ChatId)   extends BotCommand
  case class Join(chatId: ChatId)                          extends BotCommand
  case class Leave(chatId: ChatId)                         extends BotCommand
  case class SetDelay(chatId: ChatId, delay: String)       extends BotCommand
  case class ResetNickname(chatId: ChatId)                 extends BotCommand
  case class ResetDelay(chatId: ChatId)                    extends BotCommand
  case class UnknownCommand(chatId: ChatId)                extends BotCommand
  case class ShowAll(chatId: ChatId, messageId: MessageId) extends BotCommand
  case class MakeActive(chatId: ChatId, target: String)    extends BotCommand
  case class DeleteMessage(chatId: ChatId,
                           messageId: MessageId,
                           replyId: Option[MessageId])
      extends BotCommand

  case class PlainMessage(chatId: ChatId,
                          messageId: ChatId,
                          content: String,
                          replyId: Option[MessageId],
                          from: ForwardOpt)
      extends BotCommand

  case class SendMediaGroup(chatId: ChatId,
                            messageId: ChatId,
                            fileIds: List[FileId],
                            from: ForwardOpt)
      extends BotCommand

  case class SendLocation(chatId: ChatId,
                          messageId: ChatId,
                          longitude: Float,
                          latitude: Float,
                          from: ForwardOpt)
      extends BotCommand

  case class SendPhoto(chatId: ChatId,
                       messageId: ChatId,
                       fileId: FileId,
                       caption: Option[String],
                       from: ForwardOpt)
      extends BotCommand

  case class SendAudio(chatId: ChatId,
                       messageId: ChatId,
                       fileId: FileId,
                       caption: Option[String],
                       from: ForwardOpt)
      extends BotCommand

  case class SendDocument(chatId: ChatId,
                          messageId: ChatId,
                          fileId: FileId,
                          caption: Option[String],
                          from: ForwardOpt)
      extends BotCommand

  case class SendAnimation(chatId: ChatId,
                           messageId: ChatId,
                           fileId: FileId,
                           caption: Option[String],
                           from: ForwardOpt)
      extends BotCommand

  case class SendSticker(chatId: ChatId,
                         messageId: ChatId,
                         fileId: FileId,
                         from: ForwardOpt)
      extends BotCommand

  case class SendVideo(chatId: ChatId,
                       messageId: ChatId,
                       fileId: FileId,
                       caption: Option[String],
                       from: ForwardOpt)
      extends BotCommand

  case class SendVoice(chatId: ChatId,
                       messageId: ChatId,
                       fileId: FileId,
                       caption: Option[String],
                       from: ForwardOpt)
      extends BotCommand

  case class SendVideoNote(chatId: ChatId,
                           messageId: ChatId,
                           fileId: FileId,
                           from: ForwardOpt)
      extends BotCommand

  case class EditPlainMessage(chatId: ChatId,
                              messageId: MessageId,
                              newText: String)
      extends BotCommand

  case class EditCaption(chatId: ChatId,
                         messageId: MessageId,
                         newCaption: String)
      extends BotCommand

  private def fromText(m: api.dto.Message,
                       from: ForwardOpt,
                       content: String): BotCommand = {
    val messageId: ChatId          = m.messageId
    val chatId: ChatId             = m.chat.id
    val replyId: Option[MessageId] = m.replyToMessage.map(_.messageId)

    from
      .map(_ => PlainMessage(chatId, messageId, content, replyId, from))
      .getOrElse(content match {
        case `helpStr` | "/start" => ShowHelp(chatId, messageId)
        case `joinStr`            => Join(chatId)
        case `leaveStr`           => Leave(chatId)
        case setDelay(delay)      => SetDelay(chatId, delay)
        case `resetDelayStr`      => ResetDelay(chatId)
        case `resetNicknameStr`   => ResetNickname(chatId)
        case `showAllStr`         => ShowAll(chatId, messageId)
        case makeActive(target)   => MakeActive(chatId, target)
        case `deleteStr`          => DeleteMessage(chatId, messageId, replyId)
        case unknownCommand()     => UnknownCommand(chatId)
        case text                 => PlainMessage(chatId, messageId, text, replyId, None)
      })
  }

  private def fromPhoto(chatId: ChatId,
                        messageId: MessageId,
                        photos: List[PhotoSize],
                        caption: Option[String],
                        from: ForwardOpt): Option[BotCommand] =
    if (photos.isEmpty) None
    else SendPhoto(chatId, messageId, photos.head.fileId, caption, from).some

  def fromEditedMessage(m: api.dto.Message): Option[BotCommand] = {
    val editMessage =
      m.text.map(text => EditPlainMessage(m.chat.id, m.messageId, text))
    val editCaption =
      m.caption.map(caption => EditCaption(m.chat.id, m.messageId, caption))

    editCaption.orElse(editMessage)
  }

  def fromRawMessage(m: api.dto.Message): Option[BotCommand] = {
    val chatId    = m.chat.id
    val messageId = m.messageId
    val replyId   = m.replyToMessage.map(_.messageId)
    val caption   = m.caption
    val from: ForwardOpt = (m.forwardFrom, m.forwardFromChat) match {
      case (Some(user), _) => user.asLeft.some
      case (_, Some(chat)) => chat.asRight.some
      case _               => None
    }

    // TODO: media group

    lazy val photo =
      m.photo.flatMap(fromPhoto(chatId, messageId, _, caption, from))
    lazy val audio =
      m.audio.map(a => SendAudio(chatId, messageId, a.fileId, caption, from))
    lazy val document =
      m.document.map(d =>
        SendDocument(chatId, messageId, d.fileId, caption, from))
    lazy val animation =
      m.animation.map(a =>
        SendAnimation(chatId, messageId, a.fileId, caption, from))
    lazy val sticker =
      m.sticker.map(s => SendSticker(chatId, messageId, s.fileId, from))
    lazy val video =
      m.video.map(v => SendVideo(chatId, messageId, v.fileId, caption, from))
    lazy val voice =
      m.voice.map(v => SendVoice(chatId, messageId, v.fileId, caption, from))
    lazy val videoNote =
      m.videoNote.map(vn => SendVideoNote(chatId, messageId, vn.fileId, from))
    lazy val location =
      m.location.map(loc =>
        SendLocation(chatId, messageId, loc.longitude, loc.latitude, from))
    lazy val text = m.text.map(fromText(m, from, _))

    photo
      .orElse(audio)
      .orElse(animation)
      .orElse(document)
      .orElse(sticker)
      .orElse(video)
      .orElse(voice)
      .orElse(videoNote)
      .orElse(location)
      .orElse(text)
  }

  val helpStr          = "/help"
  val joinStr          = "/join"
  val leaveStr         = "/leave"
  val setDelay         = "\\/set_delay ([0-9]{1,5})".r
  val setDelayStr      = "/set_delay DELAY"
  val resetDelayStr    = "/reset_delay"
  val resetNicknameStr = "/reset_nickname"
  val unknownCommand   = "\\/.*".r
  val makeActiveStr    = "/add ID"
  val showAllStr       = "/members"
  val makeActive       = "/add (-?[0-9]{1,19})".r
  val deleteStr        = "/d"

  // TODO: ban
  // TODO: assignName
}
