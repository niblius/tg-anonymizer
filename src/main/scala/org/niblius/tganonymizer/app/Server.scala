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
  InMemoryRepositoryAlgebra
}
import org.niblius.tganonymizer.app.infrastructure.{
  DoobiePersistentRepository,
  InMemoryRepositoryInterp
}
import io.circe.generic.auto._

import scala.concurrent.ExecutionContext.Implicits.global

object Server extends IOApp {
  def createServer[F[_]: ConcurrentEffect: ContextShift: Timer]: F[Unit] = {
    def readConfiguration: F[BotConfig] =
      parser.decodePathF[F, BotConfig]("bot")

    // TODO: use env var

    def allocateResources(
        config: BotConfig): Resource[F, (Client[F], HikariTransactor[F])] =
      for {
        client <- BlazeClientBuilder[F](global).resource
        xa     <- DatabaseConfig.dbTransactor(config.db, global, global)
      } yield (client, xa)

    def launch(logger: Logger[F], config: BotConfig)(
        resources: (Client[F], HikariTransactor[F])): F[Unit] = {
      val (client, xa) = resources
      val membersRepo  = DoobiePersistentRepository(xa)
      val botAPI       = new Http4SBotAPI(config.token, client, logger)
      for {
        nicknameStore <- Ref
          .of(Map.empty[ChatId, ChatMember])
          .map(InMemoryRepositoryInterp[F](_))
        bot = new AnonymizerBot(botAPI, logger, membersRepo, nicknameStore)
        _ <- bot.launch.compile.drain
      } yield ()
    }

    for {
      logger <- Slf4jLogger.create[F]
      _      <- logger.info("Reading configuration...")
      config <- readConfiguration
      _      <- logger.info("Prepare database...")
      _      <- DatabaseConfig.initializeDb(config.db)
      _      <- logger.info("Launching the bot...")
      _      <- allocateResources(config).use(launch(logger, config))
    } yield ()
  }

  def run(args: List[String]): IO[ExitCode] =
    createServer[IO].as(ExitCode.Success)
}
