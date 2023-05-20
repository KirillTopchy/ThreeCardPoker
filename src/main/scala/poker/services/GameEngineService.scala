package poker.services

import cats.effect.{IO, Ref}
import cats.effect.std.Queue
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import poker.domain.player.PlayerId
import poker.server.{ClientMessage, ServerMessage}

import scala.concurrent.duration._

class GameEngineService(gameProcessingService: GameProcessingService)(logger: Logger[IO]) {

  def startGameEngine: IO[Unit] = gameProcessingService.firstEverWaitingForPlayers() *> loop()

  private def loop(): IO[Unit] =
    logger.info("Server is waiting for players") *>
      IO.sleep(10.seconds) *>
      gameProcessingService.joinedPlayersBeforeGame().flatMap {
        case Right(count) =>
          if (count.value > 0) {
            gameProcessingService.startNewGame() *>
              gameProcessingService.waitForDecisions() *>
              IO.sleep(10.seconds) *>
              gameProcessingService.decisionsFinished() *>
              IO.sleep(2.seconds) *>
              gameProcessingService.resolveGame() *>
              IO.sleep(2.seconds) *>
              gameProcessingService.waitNextGamePlayers() *> loop()
          } else {
            // Looks no
            logger.info("Looks like no one has joined, let's wait again ...") *>
              loop()
          }
        case Left(message) =>
          logger.error(s"Game server in wrong state: $message") *> IO
            .raiseError(new RuntimeException(s"Game server stopped: $message"))
      }
}

object GameEngineService {
  def of(gameProcessingService: GameProcessingService): IO[GameEngineService] =
    Slf4jLogger.fromName[IO]("game-engine").map { logger =>
      new GameEngineService(gameProcessingService)(logger)
    }
}