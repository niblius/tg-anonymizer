package org.niblius.tganonymizer.api.dto

import io.circe.generic.extras._

@ConfiguredJsonCodec
case class Animation(fileId: String,
                     width: Int,
                     height: Int,
                     duration: Int,
                     thumb: Option[PhotoSize],
                     fileName: Option[String],
                     mimeType: Option[String],
                     fileSize: Option[Int])
    extends TelegramDTO
