package org.niblius.tganonymizer.api

import fs2.Stream
import org.niblius.tganonymizer.api.dto.{
  BotUpdate,
  Chat,
  InputMediaPhoto,
  Message
}

import scala.language.higherKinds

/**
  * Simplified bot api algebra that exposes only APIs required for this project
  *
  * S is the streaming effect, see https://typelevel.org/blog/2018/05/09/tagless-final-streaming.html
  *
  * For the full API reference see https://core.telegram.org/bots/api
  */
trait BotAPI[F[_], S[_]] {

  /**
    * Send a message to specified chat
    */
  def sendMessage(chatId: ChatId, message: String): F[Message]

  /**
    * Stream all updated for this bot using long polling. `S[_]` is the streaming effect.
    *
    * @param fromOffset offset of the fist message to start polling from
    */
  def pollUpdates(fromOffset: Offset): S[BotUpdate]

  def getChat(chatId: ChatId): F[Option[Chat]]

  def forwardMessage(chatId: ChatId,
                     fromChatId: String,
                     messageId: MessageId): F[Message]
  def sendPhoto(chatId: ChatId, photo: String): F[Message]
  def sendAudio(chatId: ChatId, audio: String): F[Message]
  def sendDocument(chatId: ChatId, document: String): F[Message]
  def sendVideo(chatId: ChatId, video: String): F[Message]
  def sendAnimation(chatId: ChatId, animation: String): F[Message]
  def sendVoice(chatId: ChatId, voice: String): F[Message]
  def sendVideoNote(chatId: ChatId, videoNote: String): F[Message]
  def sendMediaGroup(chatId: ChatId, media: List[InputMediaPhoto]): F[Message]
  def sendLocation(chatId: ChatId,
                   longitude: Float,
                   latitude: Float): F[Message]
  def sendSticker(chatId: ChatId, voice: String): F[Message]

}

trait StreamingBotAPI[F[_]] extends BotAPI[F, Stream[F, ?]]
