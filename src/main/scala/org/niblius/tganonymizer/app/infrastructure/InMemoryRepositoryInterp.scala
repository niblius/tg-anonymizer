package org.niblius.tganonymizer.app.infrastructure

import java.util.concurrent.TimeUnit

import cats.implicits._
import cats._
import cats.effect.Timer
import cats.effect.concurrent.Ref
import org.niblius.tganonymizer.api.ChatId
import org.niblius.tganonymizer.app.domain.{
  ChatMember,
  InMemoryRepositoryAlgebra,
  SECONDS
}

case class InMemoryRepositoryInterp[F[_]: FlatMap: Timer](
    store: Ref[F, Map[ChatId, ChatMember]])
    extends InMemoryRepositoryAlgebra[F] {

  def getChatMember(chatId: ChatId): F[Option[ChatMember]] =
    store.get.map(_.get(chatId))

  // TODO: nickname generator
  def generateName: String = "@"

  private val EXPIRATION_TIME = 3600000L
  def touchNickname(chatId: ChatId,
                    name: Option[String] = None): F[ChatMember] =
    for {
      timeNow <- Timer[F].clock.realTime(TimeUnit.MILLISECONDS)
      nickname <- store.modify(m => {
        val oldMember = m.get(chatId)
        lazy val oldName =
          oldMember.filter(_.nameValidUntil < timeNow).map(_.name)
        val newName =
          name.orElse(oldName).getOrElse(generateName)
        val newTime = timeNow + EXPIRATION_TIME
        val nickname = oldMember
          .map(_.copy(name = newName, newTime))
          .getOrElse(ChatMember(newName, newTime))
        (m + (chatId -> nickname), nickname)
      })
    } yield nickname

  def resetNickname(chatId: ChatId): F[ChatMember] =
    touchNickname(chatId, Some(generateName))

  // TODO: set delay 0 equivalent to reset delay
  def setDelay(chatId: ChatId, delay: Option[SECONDS]): F[Option[ChatMember]] =
    store.modify(ms => {
      val member = ms.get(chatId).map(_.copy(delay = delay))
      val newMap = member.map(m => ms + (chatId -> m)).getOrElse(ms)
      (newMap, member)
    })

  def resetDelay(chatId: ChatId): F[Option[ChatMember]] =
    setDelay(chatId, None)
}
