package org.niblius.tganonymizer.api.dto

import io.circe.generic.extras._
import org.niblius.tganonymizer.api.FileId

@ConfiguredJsonCodec
case class PhotoSize(fileId: FileId,
                     width: Int,
                     height: Int,
                     fileSize: Option[Int])
    extends TelegramDTO
