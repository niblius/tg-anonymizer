package org.niblius.tganonymizer.api.dto

import io.circe.generic.extras._

@ConfiguredJsonCodec
case class MessageEntity(`type`: String,
                         offset: Int,
                         length: Int,
                         url: Option[String],
                         user: Option[User])
    extends TelegramDTO
