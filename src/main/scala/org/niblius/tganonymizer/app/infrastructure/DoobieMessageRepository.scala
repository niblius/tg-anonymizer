package org.niblius.tganonymizer.app.infrastructure

import cats._
import cats.data.OptionT
import cats.effect.Sync
import cats.implicits._
import doobie._
import doobie.implicits._
import org.niblius.tganonymizer.api.{ChatId, MessageId}
import org.niblius.tganonymizer.app.domain.{Message, MessageRepositoryAlgebra}

private object MessageSQL {
  def insert(message: Message): Update0 =
    sql"""
    INSERT INTO MESSAGES (CHAT_ID, MESSAGE_ID, SOURCE_CHAT_ID, SOURCE_MESSAGE_ID)
    VALUES (${message.chatId}, ${message.messageId}, ${message.source.map(
      _.chatId)}, ${message.source.map(_.messageId)})
  """.update

  def select(
      chatId: ChatId,
      messageId: MessageId): Query0[(Long, Long, Option[Long], Option[Long])] =
    sql"""
    SELECT CHAT_ID, MESSAGE_ID, SOURCE_CHAT_ID, SOURCE_MESSAGE_ID
    FROM MESSAGES
    WHERE MESSAGE_ID = $messageId AND CHAT_ID = $chatId
  """.query

  def selectBySource(
      source: Message): Query0[(Long, Long, Option[Long], Option[Long])] =
    sql"""
    SELECT CHAT_ID, MESSAGE_ID, SOURCE_CHAT_ID, SOURCE_MESSAGE_ID
    FROM MESSAGES
    WHERE SOURCE_CHAT_ID = ${source.chatId} AND SOURCE_MESSAGE_ID = ${source.messageId}
  """.query

  def deleteQuery(chatId: ChatId, messageId: MessageId): Update0 = sql"""
    DELETE FROM MESSAGES WHERE CHAT_ID = $chatId AND MESSAGE_ID = $messageId
  """.update

  def deleteBySourceQuery(source: Message): Update0 = sql"""
    DELETE FROM MESSAGES
    WHERE SOURCE_CHAT_ID = ${source.chatId} AND SOURCE_MESSAGE_ID = ${source.messageId}
  """.update
}

class DoobieMessageRepository[F[_]: Sync](val xa: Transactor[F])
    extends MessageRepositoryAlgebra[F] {
  import MessageSQL._

  private def fromTuple(t: (Long, Long, Option[Long], Option[Long])): Message =
    Message(t._1, t._2, (t._3, t._4).mapN(Message(_, _, None)))

  def create(message: Message): F[Message] =
    insert(message).run.as(message).transact(xa)

  def get(chatId: ChatId, messageId: MessageId): F[Option[Message]] =
    select(chatId, messageId).option
      .transact(xa)
      .map(_.map(fromTuple))

  def getBySource(source: Message): F[List[Message]] =
    selectBySource(source).to[List].transact(xa).map(_.map(fromTuple))

  def deleteBySource(message: Message): F[List[Message]] =
    getBySource(message).flatTap(_ =>
      deleteBySourceQuery(message).run.transact(xa))

  def delete(chatId: ChatId, messageId: MessageId): F[Option[Message]] =
    OptionT(get(chatId, messageId))
      .semiflatMap(msg =>
        deleteQuery(chatId, messageId).run.transact(xa).as(msg))
      .value
}

object DoobieMessageRepository {
  def apply[F[_]: Sync](xa: Transactor[F]): DoobieMessageRepository[F] =
    new DoobieMessageRepository(xa)
}
