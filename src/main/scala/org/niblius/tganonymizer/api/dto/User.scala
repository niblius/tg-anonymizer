package org.niblius.tganonymizer.api.dto

import io.circe.generic.extras._

@ConfiguredJsonCodec
case class User(id: Int,
                isBot: Boolean,
                firstName: String,
                lastName: Option[String],
                username: Option[String],
                languageCode: Option[String])
    extends TelegramDTO
