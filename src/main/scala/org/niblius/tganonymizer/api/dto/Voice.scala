package org.niblius.tganonymizer.api.dto

import io.circe.generic.extras._

@ConfiguredJsonCodec
case class Voice(fileId: String,
                 duration: Int,
                 mimeType: Option[String],
                 fileSize: Option[Int])
    extends TelegramDTO
