package org.niblius.tganonymizer.api.dto

import org.niblius.tganonymizer.api.ChatId

case class Chat(id: ChatId,
                title: Option[String],
                username: Option[String],
                first_name: Option[String],
                last_name: Option[String],
                `type`: String)
    extends TelegramDTO
