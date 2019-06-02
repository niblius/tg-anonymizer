package org.niblius.tganonymizer.api.dto

import cats.effect.Sync
import org.http4s.EntityDecoder
import org.http4s.circe.jsonOf
import io.circe.{Decoder, _}
import io.circe.generic.auto._

case class BotResponse[T](ok: Boolean, result: T)

object BotResponse {
  implicit def updatesDec[F[_]: Sync]
    : EntityDecoder[F, BotResponse[List[BotUpdate]]] =
    jsonOf[F, BotResponse[List[BotUpdate]]]

  implicit def chatDec[F[_]: Sync]: EntityDecoder[F, BotResponse[Chat]] =
    jsonOf[F, BotResponse[Chat]]
}
