package org.niblius.tganonymizer.app

import cats._
import cats.implicits._
import cats.effect._
import doobie.hikari.HikariTransactor
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.circe.config.parser
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import _root_.io.chrisdavenport.log4cats._
import cats.effect.concurrent.Ref
import org.niblius.tganonymizer.api.{ChatId, Http4SBotAPI}
import org.niblius.tganonymizer.app.config.{BotConfig, DatabaseConfig}
import org.niblius.tganonymizer.app.domain.{
  ChatMember,
  ChatMemberServiceAlgebra
}
import org.niblius.tganonymizer.app.infrastructure.{
  DoobieUserRepository,
  ChatMemberServiceInterp
}
import io.circe.generic.auto._

import scala.concurrent.ExecutionContext.Implicits.global

object Server extends IOApp {
  def createServer[F[_]: ConcurrentEffect: ContextShift: Timer]: F[Unit] = {
    def readConfiguration: F[BotConfig] =
      parser.decodePathF[F, BotConfig]("bot")

    def readToken: F[String] =
      Sync[F].delay(System.getenv("ANONYMIZER_BOT_TOKEN"))

    def readEnv: F[String] =
      Sync[F].delay(System.getenv("BOT_ENVIRONMENT"))

    // TODO: use separate execution contexts

    def allocateResources(
        config: DatabaseConfig): Resource[F, (Client[F], HikariTransactor[F])] =
      for {
        client <- BlazeClientBuilder[F](global).resource
        xa     <- DatabaseConfig.dbTransactor(config, global, global)
      } yield (client, xa)

    def launch(logger: Logger[F], token: String)(
        resources: (Client[F], HikariTransactor[F])): F[Unit] = {
      val (client, xa) = resources
      val usersRepo    = DoobieUserRepository(xa)
      val botAPI       = new Http4SBotAPI(token, client, logger)
      for {
        membersRepo <- Ref
          .of(Map.empty[ChatId, ChatMember])
          .map(m => ChatMemberServiceInterp[F](m, usersRepo))
        bot = new AnonymizerBot(botAPI, logger, usersRepo, membersRepo)
        _ <- bot.launch.compile.drain
      } yield ()
    }

    for {
      logger <- Slf4jLogger.create[F]
      _      <- logger.info("Reading configuration...")
      config <- readConfiguration
      token  <- readToken
      env    <- readEnv
      dbConf = if (env == "PRODUCTION")
        config.production
      else config.development
      _ <- logger.info("Prepare database...")
      _ <- DatabaseConfig.initializeDb(dbConf)
      _ <- logger.info("Launching the bot...")
      _ <- allocateResources(dbConf).use(launch(logger, token))
    } yield ()
  }

  // TODO: graceful stop

  def run(args: List[String]): IO[ExitCode] =
    createServer[IO].as(ExitCode.Success)
}
