package org.niblius.tganonymizer.api.dto

import io.circe.generic.extras._
import org.niblius.tganonymizer.api.MessageId

@ConfiguredJsonCodec
case class Message(messageId: MessageId,
                   from: Option[User],
                   date: Int,
                   chat: Chat,
                   forwardFrom: Option[User],
                   forwardFromChat: Option[Chat],
                   forwardFromMessageId: Option[MessageId],
                   forwardSignature: Option[String],
                   forwardSenderName: Option[String],
                   forwardDate: Option[Int],
                   replyToMessage: Option[Message],
                   editDate: Option[Int],
                   mediaGroupId: Option[String],
                   authorSignature: Option[String],
                   text: Option[String],
                   entities: Option[List[MessageEntity]],
                   captionEntities: Option[List[MessageEntity]],
                   audio: Option[Audio],
                   document: Option[Document],
                   animation: Option[Animation],
                   photo: Option[List[PhotoSize]],
                   sticker: Option[Sticker],
                   video: Option[Video],
                   voice: Option[Voice],
                   videoNote: Option[VideoNote],
                   caption: Option[String],
                   contact: Option[Contact],
                   location: Option[Location],
                   poll: Option[Poll])
    extends TelegramDTO
