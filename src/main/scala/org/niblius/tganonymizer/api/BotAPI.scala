package org.niblius.tganonymizer.api

import fs2.Stream
import org.niblius.tganonymizer.api.dto.{BotUpdate, Chat}

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
}

trait StreamingBotAPI[F[_]] extends BotAPI[F, Stream[F, ?]]
