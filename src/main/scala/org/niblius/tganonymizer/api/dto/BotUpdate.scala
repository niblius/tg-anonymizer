package org.niblius.tganonymizer.api.dto

case class BotUpdate(update_id: Long, message: Option[BotMessage])
    extends TelegramDTO
