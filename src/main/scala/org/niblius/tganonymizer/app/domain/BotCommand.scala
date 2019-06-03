package org.niblius.tganonymizer.app.domain

import cats.implicits._
import cats._
import cats.data.NonEmptyList
import cats.syntax._

import scala.collection.immutable.Stream
import org.niblius.tganonymizer.api
import org.niblius.tganonymizer.api.dto.PhotoSize
import org.niblius.tganonymizer.api.{ChatId, FileId, MessageId}

sealed trait BotCommand {
  def chatId: ChatId
}

object BotCommand {

  case class ShowHelp(chatId: ChatId)                      extends BotCommand
  case class Join(chatId: ChatId)                          extends BotCommand
  case class Leave(chatId: ChatId)                         extends BotCommand
  case class SetDelay(chatId: ChatId, delay: String)       extends BotCommand
  case class ResetNickname(chatId: ChatId)                 extends BotCommand
  case class ResetDelay(chatId: ChatId)                    extends BotCommand
  case class UnknownCommand(chatId: ChatId)                extends BotCommand
  case class ShowAll(chatId: ChatId)                       extends BotCommand
  case class MakeActive(chatId: ChatId, target: String)    extends BotCommand
  case class PlainMessage(chatId: ChatId, content: String) extends BotCommand

  case class ForwardMessage(chatId: ChatId, msgId: MessageId, from: ChatId)
      extends BotCommand
  case class SendMediaGroup(chatId: ChatId, fileIds: List[FileId])
      extends BotCommand
  case class SendLocation(chatId: ChatId, longitude: Float, latitude: Float)
      extends BotCommand
  case class SendPhoto(chatId: ChatId, fileId: FileId)     extends BotCommand
  case class SendAudio(chatId: ChatId, fileId: FileId)     extends BotCommand
  case class SendDocument(chatId: ChatId, fileId: FileId)  extends BotCommand
  case class SendAnimation(chatId: ChatId, fileId: FileId) extends BotCommand
  case class SendSticker(chatId: ChatId, fileId: FileId)   extends BotCommand
  case class SendVideo(chatId: ChatId, fileId: FileId)     extends BotCommand
  case class SendVoice(chatId: ChatId, fileId: FileId)     extends BotCommand
  case class SendVideoNote(chatId: ChatId, fileId: FileId) extends BotCommand

  private def fromText(msg: String, chatId: ChatId): BotCommand =
    msg match {
      case `helpStr` | "/start" => ShowHelp(chatId)
      case `joinStr`            => Join(chatId)
      case `leaveStr`           => Leave(chatId)
      case setDelay(delay)      => SetDelay(chatId, delay)
      case `resetDelayStr`      => ResetDelay(chatId)
      case `resetNicknameStr`   => ResetNickname(chatId)
      case `showAllStr`         => ShowAll(chatId: ChatId)
      case makeActive(target)   => MakeActive(chatId, target)
      case unknownCommand()     => UnknownCommand(chatId)
      case text                 => PlainMessage(chatId, text)
    }

  private def fromPhotos(chatId: ChatId,
                         photos: List[PhotoSize]): Option[BotCommand] = {
    // TODO: incorrectly recognizes single photo of different sizes as a media group
    if (photos.isEmpty) None
    else if (photos.size == 1) SendPhoto(chatId, photos.head.fileId).some
    else SendMediaGroup(chatId, photos.map(_.fileId)).some
  }

  def fromRawMessage(m: api.dto.Message): Option[BotCommand] = {
    val chatId = m.chat.id

    lazy val forward =
      (chatId.some, m.forwardFromMessageId, m.forwardFromChat.map(_.id))
        .mapN(ForwardMessage)
    lazy val photo     = m.photo.flatMap(fromPhotos(chatId, _))
    lazy val audio     = m.audio.map(a => SendAudio(chatId, a.fileId))
    lazy val document  = m.document.map(d => SendDocument(chatId, d.fileId))
    lazy val animation = m.animation.map(a => SendAnimation(chatId, a.fileId))
    lazy val sticker   = m.sticker.map(s => SendSticker(chatId, s.fileId))
    lazy val video     = m.video.map(v => SendVideo(chatId, v.fileId))
    lazy val voice     = m.voice.map(v => SendVoice(chatId, v.fileId))
    lazy val videoNote =
      m.videoNote.map(vn => SendVideoNote(chatId, vn.fileId))
    lazy val location =
      m.location.map(loc => SendLocation(chatId, loc.longitude, loc.latitude))
    lazy val text = m.text.map(fromText(_, chatId))

    forward
      .orElse(photo)
      .orElse(audio)
      .orElse(document)
      .orElse(animation)
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
