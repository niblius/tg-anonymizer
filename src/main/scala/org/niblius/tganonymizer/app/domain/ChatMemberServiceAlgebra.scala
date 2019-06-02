package org.niblius.tganonymizer.app.domain

import org.niblius.tganonymizer.api.ChatId

trait ChatMemberServiceAlgebra[F[_]] {
  def getChatMember(chatId: ChatId): F[Option[ChatMember]]
  def touch(chatId: ChatId, name: Option[String] = None): F[ChatMember]
  def resetName(chatId: ChatId): F[ChatMember]
  def setDelay(chatId: ChatId, delay: Option[SECOND]): F[Option[ChatMember]]
  def resetDelay(chatId: ChatId): F[Option[ChatMember]]
}
