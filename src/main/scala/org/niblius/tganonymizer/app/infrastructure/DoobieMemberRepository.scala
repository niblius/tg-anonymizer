package org.niblius.tganonymizer.app.infrastructure

import cats._
import cats.data.OptionT
import cats.effect.Sync
import cats.implicits._
import doobie._
import doobie.implicits._
import org.niblius.tganonymizer.app.domain.{ChatMember, MemberRepositoryAlgebra}

private object MemberSQL {
  def insert(member: ChatMember): Update0 = sql"""
    INSERT INTO CHAT_MEMBERS (ID, NICKNAME, IS_ACTIVE)
    VALUES (${member.id}, ${member.nickname}, ${member.isActive})
  """.update

  def update(member: ChatMember, id: Long): Update0 = sql"""
    UPDATE CHAT_MEMBERS
    SET NICKNAME = ${member.nickname}, IS_ACTIVE = ${member.isActive}
    WHERE ID = $id
  """.update

  def select(memberId: Long): Query0[ChatMember] = sql"""
    SELECT ID, NICKNAME, IS_ACTIVE
    FROM CHAT_MEMBERS
    WHERE ID = $memberId
  """.query

  def selectActive: Query0[ChatMember] = sql"""
    SELECT ID, NICKNAME, IS_ACTIVE
    FROM CHAT_MEMBERS
    WHERE IS_ACTIVE = TRUE
  """.query

  def delete(memberId: Long): Update0 = sql"""
    DELETE FROM CHAT_MEMBERS WHERE ID = $memberId
  """.update
}

class DoobieMemberRepositoryInterpreter[F[_]: Sync](val xa: Transactor[F])
    extends MemberRepositoryAlgebra[F] {

  import MemberSQL._

  def create(member: ChatMember): F[ChatMember] =
    insert(member).run.as(member).transact(xa)

  def update(member: ChatMember): F[ChatMember] =
    MemberSQL.update(member, member.id).run.transact(xa).as(member)

  def get(memberId: Long): F[Option[ChatMember]] =
    select(memberId).option.transact(xa)

  def delete(memberId: Long): F[Option[ChatMember]] =
    OptionT(get(memberId))
      .semiflatMap(member =>
        MemberSQL.delete(memberId).run.transact(xa).as(member))
      .value

  def getActive: F[List[ChatMember]] =
    selectActive.to[List].transact(xa)

}

object DoobieMemberRepositoryInterpreter {
  def apply[F[_]: Sync](
      xa: Transactor[F]): DoobieMemberRepositoryInterpreter[F] =
    new DoobieMemberRepositoryInterpreter(xa)
}
