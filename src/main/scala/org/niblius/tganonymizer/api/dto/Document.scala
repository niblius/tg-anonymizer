package org.niblius.tganonymizer.api.dto

import io.circe.generic.extras._

@ConfiguredJsonCodec
case class Document(fileId: String,
                    thumb: Option[PhotoSize],
                    fileName: Option[String],
                    mimeType: Option[String],
                    fileSize: Option[Int])
    extends TelegramDTO
