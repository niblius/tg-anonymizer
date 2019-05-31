package org.niblius.tganonymizer.app.domain

import org.niblius.tganonymizer.api.UserId

case class ChatMember(id: UserId, nickname: String, isActive: Boolean)
