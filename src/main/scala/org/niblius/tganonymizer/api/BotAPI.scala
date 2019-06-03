package org.niblius.tganonymizer.api

import fs2.Stream
import org.niblius.tganonymizer.api.dto.{BotUpdate, Chat, InputMediaPhoto}

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
  def sendMessage(chatId: ChatId, message: String): F[Unit]

  /**
    * Stream all updated for this bot using long polling. `S[_]` is the streaming effect.
    *
    * @param fromOffset offset of the fist message to start polling from
    */
  def pollUpdates(fromOffset: Offset): S[BotUpdate]

  def getChat(chatId: ChatId): F[Option[Chat]]

  def forwardMessage(chatId: ChatId,
                     fromChatId: String,
                     messageId: MessageId): F[Unit]
  def sendPhoto(chatId: ChatId, photo: String): F[Unit]
  def sendAudio(chatId: ChatId, audio: String): F[Unit]
  def sendDocument(chatId: ChatId, document: String): F[Unit]
  def sendVideo(chatId: ChatId, video: String): F[Unit]
  def sendAnimation(chatId: ChatId, animation: String): F[Unit]
  def sendVoice(chatId: ChatId, voice: String): F[Unit]
  def sendVideoNote(chatId: ChatId, videoNote: String): F[Unit]
  def sendMediaGroup(chatId: ChatId, media: List[InputMediaPhoto]): F[Unit]
  def sendLocation(chatId: ChatId, longitude: Float, latitude: Float): F[Unit]
  def sendSticker(chatId: ChatId, voice: String): F[Unit]

}

trait StreamingBotAPI[F[_]] extends BotAPI[F, Stream[F, ?]]
