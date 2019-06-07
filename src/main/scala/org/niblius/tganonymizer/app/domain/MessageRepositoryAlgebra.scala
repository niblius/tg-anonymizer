package org.niblius.tganonymizer.app.domain

import org.niblius.tganonymizer.api.{ChatId, MessageId}

trait MessageRepositoryAlgebra[F[_]] {
  def create(message: Message): F[Message]

  def get(chatId: ChatId, messageId: MessageId): F[Option[Message]]

  def getBySource(source: Message): F[List[Message]]

  def deleteBySource(source: Message): F[List[Message]]

  def delete(chatId: ChatId, messageId: MessageId): F[Option[Message]]
}
