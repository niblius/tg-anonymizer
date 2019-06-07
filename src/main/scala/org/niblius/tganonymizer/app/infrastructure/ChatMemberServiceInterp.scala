package org.niblius.tganonymizer.app.infrastructure

import java.util.concurrent.TimeUnit

import cats.implicits._
import cats.effect.{Sync, Timer}
import cats.effect.concurrent.Ref
import org.niblius.tganonymizer.api.ChatId
import org.niblius.tganonymizer.app.domain._

import scala.util.Random

case class ChatMemberServiceInterp[F[_]: Sync: Timer](
    store: Ref[F, Map[ChatId, ChatMember]],
    userRepo: UserRepositoryAlgebra[F])
    extends ChatMemberServiceAlgebra[F] {

  def getChatMember(chatId: ChatId): F[Option[ChatMember]] =
    store.get.map(_.get(chatId))

  // length 10
  val emoji: Vector[String] =
    Vector("\uD83D\uDE03",
           "\uD83D\uDE07",
           "\uD83D\uDE0D",
           "\uD83D\uDE21",
           "\uD83D\uDC7D",
           "\uD83D\uDE08",
           "\uD83E\uDD2A",
           "\uD83D\uDE0E",
           "\uD83D\uDC80",
           "\uD83D\uDCA9")

  def generateName(randOpt: Option[Random] = None): F[String] = {
    def generate(seed: Long, length: Int): String = {
      val seedStr = seed.toString
      val ending  = seedStr.substring(seedStr.length - length)
      ending
        .split("")
        .map(digit => emoji(digit.toInt))
        .mkString("")
    }

    val generated = for {
      rand        <- randOpt.map(Sync[F].pure).getOrElse(Sync[F].delay(new Random()))
      activeCount <- userRepo.getByIsActive(true).map(_.size)
      seed        <- Sync[F].delay(rand.nextLong())
      members     <- store.get.map(_.values)
      length   = Math.ceil(Math.log10(activeCount + 1) + 1d).toInt
      name     = generate(seed, length)
      isUnique = !members.exists(_.name == name)
    } yield (name, isUnique, rand)

    // for-comprehensions don't tail rec in current scala
    generated.flatMap(t => {
      val (name, isUnique, rand) = t
      if (isUnique) Sync[F].pure(name) else generateName(rand.some)
    })
  }

  private val EXPIRATION_TIME: SECOND = 3600L
  def touch(chatId: ChatId, nameOpt: Option[String] = None): F[ChatMember] = {
    def getName(member: Option[ChatMember], time: Long): F[String] =
      nameOpt
        .map(Sync[F].pure)
        .orElse(
          member.filter(_.nameValidUntil > time).map(_.name).map(Sync[F].pure))
        .getOrElse(generateName())

    def buildChatMember(old: Option[ChatMember],
                        name: String,
                        until: Long): ChatMember =
      old
        .map(_.copy(name = name, until))
        .getOrElse(ChatMember(name, until))

    for {
      timeNow   <- Timer[F].clock.realTime(TimeUnit.SECONDS)
      oldMember <- store.get.map(_.get(chatId))
      newName   <- getName(oldMember, timeNow)
      newMember = buildChatMember(oldMember, newName, timeNow + EXPIRATION_TIME)
      _ <- store.update(m => m + (chatId -> newMember))
    } yield newMember
  }

  def resetName(chatId: ChatId): F[ChatMember] =
    for {
      name <- generateName()
      cm   <- touch(chatId, Some(name))
    } yield cm

  def setDelay(chatId: ChatId, delay: Option[SECOND]): F[Option[ChatMember]] =
    store.modify(ms => {
      val member = ms
        .get(chatId)
        .map(_.copy(delay = delay.flatMap(d => if (d == 0L) None else Some(d))))
      val newMap = member.map(m => ms + (chatId -> m)).getOrElse(ms)
      (newMap, member)
    })

  def resetDelay(chatId: ChatId): F[Option[ChatMember]] =
    setDelay(chatId, None)
}
