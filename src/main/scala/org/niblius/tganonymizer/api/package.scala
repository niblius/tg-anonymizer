package org.niblius.tganonymizer

import io.circe.generic.extras.Configuration

package object api {
  type ChatId    = Long
  type UserId    = Long
  type Offset    = Long
  type MessageId = Long
  type FileId    = String
}
