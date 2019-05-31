package org.niblius.tganonymizer.app.domain

trait MemberRepositoryAlgebra[F[_]] {
  def create(user: ChatMember): F[ChatMember]

  def update(user: ChatMember): F[ChatMember]

  def get(userId: Long): F[Option[ChatMember]]

  def getActive: F[List[ChatMember]]

  def delete(userId: Long): F[Option[ChatMember]]
}
