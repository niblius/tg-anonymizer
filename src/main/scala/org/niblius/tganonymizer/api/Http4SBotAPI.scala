package org.niblius.tganonymizer.api

import cats.effect.Sync
import cats.implicits._
import fs2.Stream
import io.chrisdavenport.log4cats.Logger
import io.circe.Encoder
import io.circe.syntax._
import io.circe.generic.semiauto._
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s._
import org.niblius.tganonymizer.api.dto._

/**
  * Single bot API instance with http4s client.
  *
  * @param token  bot api token
  * @param client http client algebra
  * @param logger logger algebra
  */
class Http4SBotAPI[F[_]](token: String, client: Client[F], logger: Logger[F])(
    implicit
    F: Sync[F])
    extends StreamingBotAPI[F] {

  private val botApiUri: Uri  = uri"https://api.telegram.org" / s"bot$token"
  private val timeout: String = "0.5" // timeout to throttle the polling

  def sendMessage(chatId: ChatId, message: String): F[Unit] = {
    val uri = botApiUri / "sendMessage" =? Map(
      "chat_id"    -> List(chatId.toString),
      "parse_mode" -> List("Markdown"),
      "text"       -> List(message)
    )

    client.expect[Unit](uri)
  }

  def pollUpdates(fromOffset: Offset): Stream[F, BotUpdate] = {
    Stream(()).repeat
      .covary[F]
      .evalMapAccumulate(fromOffset) {
        case (offset, _) => requestUpdates(offset)
      }
      .flatMap { case (_, response) => Stream.emits(response.result) }
  }

  private def requestUpdates(
      offset: Offset): F[(Offset, BotResponse[List[BotUpdate]])] = {

    val uri = botApiUri / "getUpdates" =? Map(
      "offset"          -> List((offset + 1).toString),
      "timeout"         -> List(timeout),
      "allowed_updates" -> List("""["message"]""")
    )

    client
      .expect[BotResponse[List[BotUpdate]]](uri)
      .map(response => (lastOffset(response).getOrElse(offset), response))
      .recoverWith {
        case ex =>
          logger
            .error(ex)("Failed to poll updates")
            .as(offset -> BotResponse(ok = false, Nil))
      }
  }

  // just get the maximum id out of all received updates
  private def lastOffset(
      response: BotResponse[List[BotUpdate]]): Option[Offset] =
    response.result match {
      case Nil      => None
      case nonEmpty => Some(nonEmpty.maxBy(_.updateId).updateId)
    }

  def getChat(chatId: ChatId): F[Option[Chat]] = {
    val uri = botApiUri / "getChat" =? Map(
      "chat_id" -> List(chatId)
    )

    client
      .expect[BotResponse[Chat]](uri)
      .map(_.result.some)
      .recoverWith {
        case ex =>
          logger
            .error(ex)("Failed to get chat")
            .as(None)
      }
  }

  def forwardMessage(chatId: ChatId,
                     fromChatId: String,
                     messageId: MessageId): F[Unit] = {
    val uri = botApiUri / "forwardMessage" =? Map(
      "chat_id"      -> List(chatId.toString),
      "from_chat_id" -> List(fromChatId),
      "message_id"   -> List(messageId.toString)
    )

    println(uri.toString())

    client.expect[Unit](uri)
  }

  def sendPhoto(chatId: ChatId, photo: String): F[Unit] = {
    val uri = botApiUri / "sendPhoto" =? Map(
      "chat_id" -> List(chatId.toString),
      "photo"   -> List(photo)
    )

    client.expect[Unit](uri)
  }

  def sendAudio(chatId: ChatId, audio: String): F[Unit] = {
    val uri = botApiUri / "sendAudio" =? Map(
      "chat_id" -> List(chatId.toString),
      "audio"   -> List(audio)
    )

    client.expect[Unit](uri)
  }

  def sendDocument(chatId: ChatId, document: String): F[Unit] = {
    val uri = botApiUri / "sendDocument" =? Map(
      "chat_id"  -> List(chatId.toString),
      "document" -> List(document)
    )

    client.expect[Unit](uri)
  }

  def sendVideo(chatId: ChatId, video: String): F[Unit] = {
    val uri = botApiUri / "sendVideo" =? Map(
      "chat_id" -> List(chatId.toString),
      "video"   -> List(video)
    )

    client.expect[Unit](uri)
  }

  def sendAnimation(chatId: ChatId, animation: String): F[Unit] = {
    val uri = botApiUri / "sendAnimation" =? Map(
      "chat_id"   -> List(chatId.toString),
      "animation" -> List(animation)
    )

    client.expect[Unit](uri)
  }

  def sendVoice(chatId: ChatId, voice: String): F[Unit] = {
    val uri = botApiUri / "sendVoice" =? Map(
      "chat_id" -> List(chatId.toString),
      "voice"   -> List(voice)
    )

    client.expect[Unit](uri)
  }

  def sendVideoNote(chatId: ChatId, videoNote: String): F[Unit] = {
    val uri = botApiUri / "sendVideoNote" =? Map(
      "chat_id"    -> List(chatId.toString),
      "video_note" -> List(videoNote)
    )

    client.expect[Unit](uri)
  }

  def sendSticker(chatId: ChatId, sticker: String): F[Unit] = {
    val uri = botApiUri / "sendSticker" =? Map(
      "chat_id" -> List(chatId.toString),
      "sticker" -> List(sticker)
    )

    client.expect[Unit](uri)
  }

  private case class SendMediaGroupReq(chat_id: String,
                                       media: List[InputMediaPhoto])

  private implicit val sendMediaGroupReqDec: Encoder[SendMediaGroupReq] =
    deriveEncoder

  def sendMediaGroup(chatId: ChatId, media: List[InputMediaPhoto]): F[Unit] = {

    val uri = botApiUri / "sendMediaGroup"
    val req = Request[F]()
      .withMethod(Method.GET)
      .withUri(uri)
      .withEntity(SendMediaGroupReq(chatId.toString, media).asJson)

    client.expect[Unit](req)
  }

  private case class SendLocationReq(chat_id: String,
                                     longitude: Float,
                                     latitude: Float)
  private implicit val sendLocationReqDec: Encoder[SendLocationReq] =
    deriveEncoder

  def sendLocation(chatId: ChatId,
                   latitude: Float,
                   longitude: Float): F[Unit] = {
    val uri = botApiUri / "sendLocation"

    val req = Request[F]()
      .withMethod(Method.GET)
      .withUri(uri)
      .withEntity(SendLocationReq(chatId.toString, latitude, longitude).asJson)

    client.expect(req)
  }
}
