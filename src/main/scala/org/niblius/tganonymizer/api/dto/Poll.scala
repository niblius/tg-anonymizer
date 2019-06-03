package org.niblius.tganonymizer.api.dto

import io.circe.generic.extras._

@ConfiguredJsonCodec
case class Poll(id: String,
                question: String,
                options: List[PollOption],
                isClosed: Boolean)
    extends TelegramDTO
