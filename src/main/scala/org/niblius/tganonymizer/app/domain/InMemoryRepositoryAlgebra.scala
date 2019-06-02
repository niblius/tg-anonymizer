package org.niblius.tganonymizer.app.domain

import org.niblius.tganonymizer.api.ChatId

trait InMemoryRepositoryAlgebra[F[_]] {
  def getChatMember(chatId: ChatId): F[Option[ChatMember]]
  def touchNickname(chatId: ChatId, name: Option[String] = None): F[ChatMember]
  def resetNickname(chatId: ChatId): F[ChatMember]
  def setDelay(chatId: ChatId, delay: Option[SECONDS]): F[Option[ChatMember]]
  def resetDelay(chatId: ChatId): F[Option[ChatMember]]
}
