package org.niblius.tganonymizer.app.infrastructure

import cats._
import cats.data.OptionT
import cats.effect.Sync
import cats.implicits._
import doobie._
import doobie.implicits._
import org.niblius.tganonymizer.app.domain.{
  ChatMemberSettings,
  PersistentRepositoryAlgebra
}

private object MemberSQL {
  def insert(member: ChatMemberSettings): Update0 = sql"""
    INSERT INTO CHAT_MEMBERS (ID, NICKNAME, IS_ACTIVE)
    VALUES (${member.id}, ${member.nickname}, ${member.isActive})
  """.update

  def update(member: ChatMemberSettings, id: Long): Update0 = sql"""
    UPDATE CHAT_MEMBERS
    SET NICKNAME = ${member.nickname}, IS_ACTIVE = ${member.isActive}
    WHERE ID = $id
  """.update

  def select(memberId: Long): Query0[ChatMemberSettings] = sql"""
    SELECT ID, NICKNAME, IS_ACTIVE
    FROM CHAT_MEMBERS
    WHERE ID = $memberId
  """.query

  def selectByStatus(isActive: Boolean): Query0[ChatMemberSettings] = sql"""
    SELECT ID, NICKNAME, IS_ACTIVE
    FROM CHAT_MEMBERS
    WHERE IS_ACTIVE = $isActive
  """.query

  def delete(memberId: Long): Update0 = sql"""
    DELETE FROM CHAT_MEMBERS WHERE ID = $memberId
  """.update

  def selectAll: Query0[ChatMemberSettings] = sql"""
    SELECT ID, NICKNAME, IS_ACTIVE
    FROM CHAT_MEMBERS
  """.query
}

class DoobiePersistentRepository[F[_]: Sync](val xa: Transactor[F])
    extends PersistentRepositoryAlgebra[F] {

  import MemberSQL._

  def getAll: F[List[ChatMemberSettings]] =
    selectAll.to[List].transact(xa)

  def getByStatus(active: Boolean): F[List[ChatMemberSettings]] =
    selectByStatus(active).to[List].transact(xa)

  def create(member: ChatMemberSettings): F[ChatMemberSettings] =
    insert(member).run.as(member).transact(xa)

  def update(member: ChatMemberSettings): F[ChatMemberSettings] =
    MemberSQL.update(member, member.id).run.transact(xa).as(member)

  def get(memberId: Long): F[Option[ChatMemberSettings]] =
    select(memberId).option.transact(xa)

  def delete(memberId: Long): F[Option[ChatMemberSettings]] =
    OptionT(get(memberId))
      .semiflatMap(member =>
        MemberSQL.delete(memberId).run.transact(xa).as(member))
      .value

  def getActive: F[List[ChatMemberSettings]] =
    selectByStatus(true).to[List].transact(xa)

}

object DoobiePersistentRepository {
  def apply[F[_]: Sync](xa: Transactor[F]): DoobiePersistentRepository[F] =
    new DoobiePersistentRepository(xa)
}
