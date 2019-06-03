package org.niblius.tganonymizer.api.dto

import io.circe.generic.extras._

@ConfiguredJsonCodec
case class PollOption(text: String, voterCount: Int) extends TelegramDTO
