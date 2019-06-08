package org.niblius.tganonymizer.api.dto

import io.circe.generic.extras._

@ConfiguredJsonCodec
case class BotUpdate(updateId: Long,
                     message: Option[Message],
                     editedMessage: Option[Message])
    extends TelegramDTO
