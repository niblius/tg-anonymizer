package org.niblius.tganonymizer.app

import cats._
import cats.implicits._
import cats.effect.{
  ConcurrentEffect,
  ContextShift,
  ExitCode,
  IO,
  IOApp,
  Resource,
  Sync
}
import fs2.Stream
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.circe.generic.auto._
import io.circe.config.parser
import org.http4s._
import org.http4s.circe._
import org.http4s.client.blaze.BlazeClientBuilder
import org.niblius.tganonymizer.api.Http4SBotAPI
import org.niblius.tganonymizer.api.dto.{BotResponse, BotUpdate}
import org.niblius.tganonymizer.app.config.{BotConfig, DatabaseConfig}

import scala.concurrent.ExecutionContext.Implicits.global

object Server extends IOApp {
  def createServer[F[_]: ConcurrentEffect: ContextShift]: F[Unit] = {
    implicit val decoder: EntityDecoder[F, BotResponse[List[BotUpdate]]] =
      jsonOf[F, BotResponse[List[BotUpdate]]]

    BlazeClientBuilder[F](global).resource.use(client => {
      for {
        logger   <- Slf4jLogger.create[F]
        token    <- Sync[F].delay(System.getenv("ANONYMIZER_BOT_TOKEN"))
        config   <- parser.decodePathF[F, BotConfig]("bot")
        database <- DatabaseConfig.dbTransactor(config.db)
        userRepository = null
        botAPI         = new Http4SBotAPI(token, client, logger)
        bot            = new AnonymizerBot(botAPI, logger)
        _ <- logger.info("Starting the bot...")
        _ <- bot.launch.compile.drain
      } yield ()
    })
  }

  def run(args: List[String]): IO[ExitCode] =
    createServer[IO].as(ExitCode.Success)
}
