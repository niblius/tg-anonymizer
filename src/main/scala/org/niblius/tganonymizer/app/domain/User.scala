package org.niblius.tganonymizer.app.domain

import org.niblius.tganonymizer.api.ChatId

case class User(chatId: ChatId, isActive: Boolean)
