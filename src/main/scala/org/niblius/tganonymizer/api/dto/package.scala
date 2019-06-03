package org.niblius.tganonymizer.api

import io.circe.generic.extras.Configuration

package object dto {
  trait TelegramDTO
  implicit val config: Configuration =
    Configuration.default.withSnakeCaseMemberNames
}
