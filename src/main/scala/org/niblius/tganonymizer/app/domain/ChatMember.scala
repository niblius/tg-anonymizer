package org.niblius.tganonymizer.app.domain

case class ChatMember(name: String,
                      nameValidUntil: MILLISECOND,
                      delay: Option[SECOND] = None)
