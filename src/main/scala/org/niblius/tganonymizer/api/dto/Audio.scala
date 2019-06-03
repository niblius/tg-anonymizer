package org.niblius.tganonymizer.api.dto

import io.circe.generic.extras._

@ConfiguredJsonCodec
case class Audio(fileId: String,
                 duration: Int,
                 performer: Option[String],
                 title: Option[String],
                 mimeType: Option[String],
                 fileSize: Option[Int],
                 thumb: Option[PhotoSize])
    extends TelegramDTO
