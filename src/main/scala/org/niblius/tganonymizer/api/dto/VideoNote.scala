package org.niblius.tganonymizer.api.dto

import io.circe.generic.extras._

@ConfiguredJsonCodec
case class VideoNote(fileId: String,
                     duration: Int,
                     thumb: Option[PhotoSize],
                     fileSize: Option[Int])
