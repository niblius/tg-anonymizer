package org.niblius.tganonymizer.api.dto

import io.circe.generic.extras._

@ConfiguredJsonCodec
case class Sticker(fileId: String,
                   width: Int,
                   height: Int,
                   thumb: Option[PhotoSize],
                   emoji: Option[String],
                   setName: Option[String],
                   maskPosition: Option[MaskPosition],
                   fileSize: Option[Int])
    extends TelegramDTO
