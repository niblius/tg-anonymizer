package org.niblius.tganonymizer.api

import java.io.IOError

import cats.effect.Sync
import cats.implicits._
import fs2.Stream
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s._
import io.chrisdavenport.log4cats.Logger
import io.circe.{Decoder, Encoder}
import io.circe.syntax._
import io.circe.generic.auto._
import org.niblius.tganonymizer.api.dto._
import org.niblius.tganonymizer.api.dto.Message

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

  private case class SendMessageReq(chat_id: String,
                                    parse_more: String,
                                    text: String)

  private implicit val apiErrorEntityDec: EntityDecoder[F, ApiError] =
    jsonOf

  private def logApiError(msg: String): Response[F] => F[Throwable] =
    (error: Response[F]) =>
      logger
        .error(s"$msg:\n${error.toString()}")
        .flatMap(_ => error.as[ApiError].map(throw _))

  private implicit val apiMessageEntityDec
    : EntityDecoder[F, BotResponse[Message]] =
    jsonOf

  def sendMessage(chatId: ChatId, message: String): F[Message] = {
    val uri = botApiUri / "sendMessage"

    val data = SendMessageReq(chatId.toString, "Markdown", message)

    val req = Request[F]()
      .withMethod(Method.POST)
      .withUri(uri)
      .withEntity(data.asJson)

    client
      .expectOr[BotResponse[Message]](req)(
        logApiError("Failed to send message"))
      .map(_.result)
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
      .expectOr[BotResponse[List[BotUpdate]]](uri)(error =>
        error.as[ApiError].map(throw _))
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
      .expectOr[BotResponse[Chat]](uri)(error =>
        error.as[ApiError].map(throw _))
      .map(resp => resp.result.some)
      .recover {
        case ApiError(_, 404, _) => None: Option[Chat]
      }
      .onError { case ex => logger.error(ex)("Failed to get chat") }
  }

  def forwardMessage(chatId: ChatId,
                     fromChatId: String,
                     messageId: MessageId): F[Message] = {
    val uri = botApiUri / "forwardMessage" =? Map(
      "chat_id"      -> List(chatId.toString),
      "from_chat_id" -> List(fromChatId),
      "message_id"   -> List(messageId.toString)
    )

    client
      .expectOr[BotResponse[Message]](uri)(
        logApiError("Failed to forward message"))
      .map(_.result)
  }

  def sendPhoto(chatId: ChatId, photo: String): F[Message] = {
    val uri = botApiUri / "sendPhoto" =? Map(
      "chat_id" -> List(chatId.toString),
      "photo"   -> List(photo)
    )

    client
      .expectOr[BotResponse[Message]](uri)(logApiError("Failed to send photo"))
      .map(_.result)
  }

  def sendAudio(chatId: ChatId, audio: String): F[Message] = {
    val uri = botApiUri / "sendAudio" =? Map(
      "chat_id" -> List(chatId.toString),
      "audio"   -> List(audio)
    )

    client
      .expectOr[BotResponse[Message]](uri)(logApiError("Failed to send audio"))
      .map(_.result)
  }

  def sendDocument(chatId: ChatId, document: String): F[Message] = {
    val uri = botApiUri / "sendDocument" =? Map(
      "chat_id"  -> List(chatId.toString),
      "document" -> List(document)
    )

    client
      .expectOr[BotResponse[Message]](uri)(
        logApiError("Failed to send document"))
      .map(_.result)
  }

  def sendVideo(chatId: ChatId, video: String): F[Message] = {
    val uri = botApiUri / "sendVideo" =? Map(
      "chat_id" -> List(chatId.toString),
      "video"   -> List(video)
    )

    client
      .expectOr[BotResponse[Message]](uri)(logApiError("Failed to end video"))
      .map(_.result)
  }

  def sendAnimation(chatId: ChatId, animation: String): F[Message] = {
    val uri = botApiUri / "sendAnimation" =? Map(
      "chat_id"   -> List(chatId.toString),
      "animation" -> List(animation)
    )

    client
      .expectOr[BotResponse[Message]](uri)(
        logApiError("Failed to send animaiton"))
      .map(_.result)
  }

  def sendVoice(chatId: ChatId, voice: String): F[Message] = {
    val uri = botApiUri / "sendVoice" =? Map(
      "chat_id" -> List(chatId.toString),
      "voice"   -> List(voice)
    )

    client
      .expectOr[BotResponse[Message]](uri)(logApiError("Failed to send voice"))
      .map(_.result)
  }

  def sendVideoNote(chatId: ChatId, videoNote: String): F[Message] = {
    val uri = botApiUri / "sendVideoNote" =? Map(
      "chat_id"    -> List(chatId.toString),
      "video_note" -> List(videoNote)
    )

    client
      .expectOr[BotResponse[Message]](uri)(
        logApiError("Failed to send video note"))
      .map(_.result)
  }

  def sendSticker(chatId: ChatId, sticker: String): F[Message] = {
    val uri = botApiUri / "sendSticker" =? Map(
      "chat_id" -> List(chatId.toString),
      "sticker" -> List(sticker)
    )

    client
      .expectOr[BotResponse[Message]](uri)(
        logApiError("Failed to send sticker"))
      .map(_.result)
  }

  private case class SendMediaGroupReq(chat_id: String,
                                       media: List[InputMediaPhoto])

  def sendMediaGroup(chatId: ChatId,
                     media: List[InputMediaPhoto]): F[Message] = {

    val uri = botApiUri / "sendMediaGroup"
    val req = Request[F]()
      .withMethod(Method.GET)
      .withUri(uri)
      .withEntity(SendMediaGroupReq(chatId.toString, media).asJson)

    client
      .expectOr[BotResponse[Message]](req)(
        logApiError("Failed to send media group"))
      .map(_.result)
  }

  private case class SendLocationReq(chat_id: String,
                                     longitude: Float,
                                     latitude: Float)

  def sendLocation(chatId: ChatId,
                   latitude: Float,
                   longitude: Float): F[Message] = {
    val uri = botApiUri / "sendLocation"

    val req = Request[F]()
      .withMethod(Method.GET)
      .withUri(uri)
      .withEntity(SendLocationReq(chatId.toString, latitude, longitude).asJson)

    client
      .expectOr[BotResponse[Message]](req)(
        logApiError("Failed to send location"))
      .map(_.result)
  }
}
