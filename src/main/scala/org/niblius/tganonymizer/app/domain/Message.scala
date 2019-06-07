package org.niblius.tganonymizer.app.domain

import org.niblius.tganonymizer.api.{ChatId, MessageId}

case class Message(chatId: ChatId,
                   messageId: MessageId,
                   source: Option[Message] = None)
