package org.niblius.tganonymizer.app.domain

trait PersistentRepositoryAlgebra[F[_]] {
  def create(user: ChatMemberSettings): F[ChatMemberSettings]

  def update(user: ChatMemberSettings): F[ChatMemberSettings]

  def get(userId: Long): F[Option[ChatMemberSettings]]

  def getByStatus(active: Boolean): F[List[ChatMemberSettings]]

  def getAll: F[List[ChatMemberSettings]]

  def delete(userId: Long): F[Option[ChatMemberSettings]]
}
