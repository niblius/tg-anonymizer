package org.niblius.tganonymizer.api.dto

import io.circe.generic.extras._

@ConfiguredJsonCodec
case class Location(longitude: Float, latitude: Float) extends TelegramDTO
