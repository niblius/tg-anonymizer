package org.niblius.tganonymizer.app.infrastructure

import cats._
import cats.data.OptionT
import cats.effect.Sync
import cats.implicits._
import doobie._
import doobie.implicits._
import org.niblius.tganonymizer.api.ChatId
import org.niblius.tganonymizer.app.domain.{User, UserRepositoryAlgebra}

private object UserSQL {
  def insert(user: User): Update0 = sql"""
    INSERT INTO USERS (CHAT_ID, IS_ACTIVE)
    VALUES (${user.chatId}, ${user.isActive})
  """.update

  def update(user: User, id: Long): Update0 = sql"""
    UPDATE USERS
    SET IS_ACTIVE = ${user.isActive}
    WHERE CHAT_ID = $id
  """.update

  def select(chatId: Long): Query0[User] = sql"""
    SELECT CHAT_ID, IS_ACTIVE
    FROM USERS
    WHERE CHAT_ID = $chatId
  """.query

  def selectByIsActive(isActive: Boolean): Query0[User] = sql"""
    SELECT CHAT_ID, IS_ACTIVE
    FROM USERS
    WHERE IS_ACTIVE = $isActive
  """.query

  def delete(chatId: Long): Update0 = sql"""
    DELETE FROM USERS WHERE CHAT_ID = $chatId
  """.update

  def selectAll: Query0[User] = sql"""
    SELECT CHAT_ID, IS_ACTIVE
    FROM USERS
  """.query
}

class DoobieUserRepository[F[_]: Sync](val xa: Transactor[F])
    extends UserRepositoryAlgebra[F] {

  import UserSQL._

  def getAll: F[List[User]] =
    selectAll.to[List].transact(xa)

  def getByIsActive(active: Boolean): F[List[User]] =
    selectByIsActive(active).to[List].transact(xa)

  def create(user: User): F[User] =
    insert(user).run.as(user).transact(xa)

  def update(user: User): F[User] =
    UserSQL.update(user, user.chatId).run.transact(xa).as(user)

  def get(chatId: ChatId): F[Option[User]] =
    select(chatId).option.transact(xa)

  def delete(chatId: ChatId): F[Option[User]] =
    OptionT(get(chatId))
      .semiflatMap(user => UserSQL.delete(chatId).run.transact(xa).as(user))
      .value

  def getActive: F[List[User]] =
    selectByIsActive(true).to[List].transact(xa)

}

object DoobieUserRepository {
  def apply[F[_]: Sync](xa: Transactor[F]): DoobieUserRepository[F] =
    new DoobieUserRepository(xa)
}
