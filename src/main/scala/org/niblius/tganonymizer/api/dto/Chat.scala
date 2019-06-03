package org.niblius.tganonymizer.api.dto

import org.niblius.tganonymizer.api.ChatId
import io.circe.generic.extras._

@ConfiguredJsonCodec
case class Chat(id: ChatId,
                title: Option[String],
                username: Option[String],
                firstName: Option[String],
                lastName: Option[String],
                `type`: String)
    extends TelegramDTO
