package org.niblius.tganonymizer.api.dto

import io.circe.generic.extras._

@ConfiguredJsonCodec
case class MaskPosition(point: String,
                        xShift: Float,
                        yShift: Float,
                        scale: Float)
    extends TelegramDTO
