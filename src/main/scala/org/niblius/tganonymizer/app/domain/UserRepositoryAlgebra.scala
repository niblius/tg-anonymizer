package org.niblius.tganonymizer.app.domain

import org.niblius.tganonymizer.api.ChatId

trait UserRepositoryAlgebra[F[_]] {
  def create(user: User): F[User]

  def update(user: User): F[User]

  def get(chatId: ChatId): F[Option[User]]

  def getAll: F[List[User]]

  def getByIsActive(active: Boolean): F[List[User]]

  def delete(chatId: ChatId): F[Option[User]]
}
