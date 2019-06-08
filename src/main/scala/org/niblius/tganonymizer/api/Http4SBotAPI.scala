package org.niblius.tganonymizer.api

import java.io.IOError

import cats.effect.Sync
import cats._
import cats.data._
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
                                    text: String,
                                    reply_to_message_id: Option[MessageId])

  private implicit val apiErrorEntityDec: EntityDecoder[F, ApiError] =
    jsonOf

  private def logApiError(msg: String): Response[F] => F[Throwable] =
    (error: Response[F]) =>
      error
        .as[ApiError]
        .flatTap(apiError =>
          logger
            .error(s"$msg:\n${apiError.toString}"))
        .map(throw _)

  private implicit val apiMessageEntityDec
    : EntityDecoder[F, BotResponse[Message]] =
    jsonOf

  private def makeApiRequestRaw[R[_], A](req: Request[F])(errorMsg: String)(
      implicit dec: EntityDecoder[F, R[A]]): F[Either[ApiError, R[A]]] =
    client
      .expectOr[R[A]](req)(logApiError(errorMsg))
      .map(_.asRight[ApiError])
      .recover { case error: ApiError => error.asLeft[R[A]] }

  private def makeApiRequestRaw[R[_], A](uri: Uri)(errorMsg: String)(
      implicit dec: EntityDecoder[F, R[A]]): F[Either[ApiError, R[A]]] = {
    val req = Request[F]().withMethod(Method.GET).withUri(uri)
    makeApiRequestRaw[R, A](req)(errorMsg)
  }

  private def makeRequest[A](uri: Uri)(errorMsg: String)(
      implicit dec: EntityDecoder[F, BotResponse[A]]): F[Either[ApiError, A]] =
    makeApiRequestRaw[BotResponse, A](uri)(errorMsg)
      .map(_.flatMap(_.result.asRight))

  private def makeRequest[A](req: Request[F])(errorMsg: String)(
      implicit dec: EntityDecoder[F, BotResponse[A]]): F[Either[ApiError, A]] =
    makeApiRequestRaw[BotResponse, A](req)(errorMsg)
      .map(_.flatMap(_.result.asRight))

  def sendMessage(
      chatId: ChatId,
      message: String,
      replyId: Option[MessageId] = None): F[Either[ApiError, Message]] = {
    val uri = botApiUri / "sendMessage"

    val data = SendMessageReq(chatId.toString, "Markdown", message, replyId)

    val req = Request[F]()
      .withMethod(Method.POST)
      .withUri(uri)
      .withEntity(data.asJson)

    makeRequest(req)(s"Failed to send message to $chatId")
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
      "allowed_updates" -> List("""["message", "edited_message"]""")
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

  def getChat(chatId: ChatId): F[Either[ApiError, Chat]] = {
    val uri = botApiUri / "getChat" =? Map(
      "chat_id" -> List(chatId)
    )

    makeRequest(uri)(s"Failed to get chat with id $chatId")
  }

  def forwardMessage(chatId: ChatId,
                     fromChatId: String,
                     messageId: MessageId): F[Either[ApiError, Message]] = {
    val uri = botApiUri / "forwardMessage" =? Map(
      "chat_id"      -> List(chatId.toString),
      "from_chat_id" -> List(fromChatId),
      "message_id"   -> List(messageId.toString)
    )

    makeRequest(uri)("Failed to forward message")
  }

  def sendPhoto(chatId: ChatId,
                photo: String,
                caption: Option[String]): F[Either[ApiError, Message]] = {
    val uri = botApiUri / "sendPhoto" =? Map(
      "chat_id" -> List(chatId.toString),
      "photo"   -> List(photo),
      "caption" -> List(caption.getOrElse(""))
    )

    makeRequest(uri)("Failed to send photo")
  }

  def sendAudio(chatId: ChatId,
                audio: String,
                caption: Option[String]): F[Either[ApiError, Message]] = {
    val uri = botApiUri / "sendAudio" =? Map(
      "chat_id" -> List(chatId.toString),
      "audio"   -> List(audio),
      "caption" -> List(caption.getOrElse(""))
    )

    makeRequest(uri)("Failed to send audio")
  }

  def sendDocument(chatId: ChatId,
                   document: String,
                   caption: Option[String]): F[Either[ApiError, Message]] = {
    val uri = botApiUri / "sendDocument" =? Map(
      "chat_id"  -> List(chatId.toString),
      "document" -> List(document),
      "caption"  -> List(caption.getOrElse(""))
    )

    makeRequest(uri)("Failed to send document")
  }

  def sendVideo(chatId: ChatId,
                video: String,
                caption: Option[String]): F[Either[ApiError, Message]] = {
    val uri = botApiUri / "sendVideo" =? Map(
      "chat_id" -> List(chatId.toString),
      "video"   -> List(video),
      "caption" -> List(caption.getOrElse(""))
    )

    makeRequest(uri)("Failed to send video")
  }

  def sendAnimation(chatId: ChatId,
                    animation: String,
                    caption: Option[String]): F[Either[ApiError, Message]] = {
    val uri = botApiUri / "sendAnimation" =? Map(
      "chat_id"   -> List(chatId.toString),
      "animation" -> List(animation),
      "caption"   -> List(caption.getOrElse(""))
    )

    makeRequest(uri)("Failed to send animation")
  }

  def sendVoice(chatId: ChatId,
                voice: String,
                caption: Option[String]): F[Either[ApiError, Message]] = {
    val uri = botApiUri / "sendVoice" =? Map(
      "chat_id" -> List(chatId.toString),
      "voice"   -> List(voice),
      "caption" -> List(caption.getOrElse(""))
    )

    makeRequest(uri)("Failed to send voice")
  }

  def sendVideoNote(chatId: ChatId,
                    videoNote: String): F[Either[ApiError, Message]] = {
    val uri = botApiUri / "sendVideoNote" =? Map(
      "chat_id"    -> List(chatId.toString),
      "video_note" -> List(videoNote)
    )

    makeRequest(uri)("Failed to send video note")
  }

  def sendSticker(chatId: ChatId,
                  sticker: String): F[Either[ApiError, Message]] = {
    val uri = botApiUri / "sendSticker" =? Map(
      "chat_id" -> List(chatId.toString),
      "sticker" -> List(sticker)
    )

    makeRequest(uri)("Failed to send sticker")
  }

  private case class SendMediaGroupReq(chat_id: String,
                                       media: List[InputMediaPhoto])

  def sendMediaGroup(
      chatId: ChatId,
      media: List[InputMediaPhoto]): F[Either[ApiError, Message]] = {

    val uri = botApiUri / "sendMediaGroup"
    val req = Request[F]()
      .withMethod(Method.GET)
      .withUri(uri)
      .withEntity(SendMediaGroupReq(chatId.toString, media).asJson)

    makeRequest(req)("Failed to send media group")
  }

  private case class SendLocationReq(chat_id: String,
                                     longitude: Float,
                                     latitude: Float)

  def sendLocation(chatId: ChatId,
                   latitude: Float,
                   longitude: Float): F[Either[ApiError, Message]] = {
    val uri = botApiUri / "sendLocation"

    val req = Request[F]()
      .withMethod(Method.GET)
      .withUri(uri)
      .withEntity(SendLocationReq(chatId.toString, latitude, longitude).asJson)

    makeRequest(req)("Failed to send location")
  }

  private implicit val quickResultEntityDec
    : EntityDecoder[F, QuickResult[Unit]] =
    jsonOf

  def deleteMessage(
      chatId: ChatId,
      messageId: MessageId): F[Either[ApiError, QuickResult[Unit]]] = {
    val uri = botApiUri / "deleteMessage" =? Map(
      "chat_id"    -> List(chatId.toString),
      "message_id" -> List(messageId.toString))

    val errorMsg = s"Failed to delete a message $messageId in chat $chatId"

    makeApiRequestRaw[QuickResult, Unit](uri)(errorMsg)
  }

  case class EditMessageTextReq(chat_id: ChatId,
                                message_id: MessageId,
                                text: String)

  def editMessageText(
      chatId: ChatId,
      messageId: MessageId,
      newText: String): F[Either[ApiError, QuickResult[Unit]]] = {
    val uri = botApiUri / "editMessageText"

    val req = Request[F]()
      .withMethod(Method.POST)
      .withUri(uri)
      .withEntity(EditMessageTextReq(chatId, messageId, newText).asJson)

    val errorMsg = s"Failed to edit a message $messageId in the chat $chatId"

    makeApiRequestRaw[QuickResult, Unit](req)(errorMsg)
  }

  case class EditMessageCaptionReq(chat_id: ChatId,
                                   message_id: MessageId,
                                   caption: String)

  def editMessageCaption(
      chatId: ChatId,
      messageId: MessageId,
      newText: String): F[Either[ApiError, QuickResult[Unit]]] = {
    val uri = botApiUri / "editMessageCaption"

    val req = Request[F]()
      .withMethod(Method.POST)
      .withUri(uri)
      .withEntity(EditMessageCaptionReq(chatId, messageId, newText).asJson)

    val errorMsg =
      s"Failed to edit a caption of the message $messageId in the chat $chatId"

    makeApiRequestRaw[QuickResult, Unit](req)(errorMsg)
  }
}
