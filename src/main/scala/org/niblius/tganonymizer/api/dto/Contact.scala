package org.niblius.tganonymizer.api.dto

import io.circe.generic.extras._

@ConfiguredJsonCodec
case class Contact(phoneNumber: String,
                   firstName: String,
                   lastName: Option[String],
                   userId: Option[Int],
                   vcard: Option[String])
    extends TelegramDTO
