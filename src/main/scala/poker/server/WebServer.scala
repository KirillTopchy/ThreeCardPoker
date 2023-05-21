package poker.server

import cats.effect.std.Queue
import cats.effect.{ExitCode, IO, IOApp, Ref}
import cats.implicits.catsSyntaxApplicativeId
import com.comcast.ip4s._
import org.http4s.HttpApp
import org.http4s.ember.server._
import org.http4s.server.websocket.WebSocketBuilder2
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import poker.domain.card.Deck
import poker.domain.game.GameState
import poker.domain.player.PlayerId
import poker.services.{
  ClientMessageProcessingService,
  GameEngineService,
  GameProcessingService,
  GameServerMessageService
}

import scala.concurrent.duration.DurationInt

object WebServer extends IOApp {
  private def httpApp(
    refOutMessageQueues: Ref[IO, Map[PlayerId, Queue[IO, ServerMessage]]],
    gameProcessingService: GameProcessingService,
    clientMessageProcessingService: ClientMessageProcessingService,
    logger: SelfAwareStructuredLogger[IO]
  )(wsb: WebSocketBuilder2[IO]): HttpApp[IO] = {
    val routes = new PokerRoutes(
      refOutMessageQueues,
      gameProcessingService,
      clientMessageProcessingService,
      logger
    )
    routes.createRoute(wsb)
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    for {
      refOutMessageQueues <- Ref
        .of[IO, Map[PlayerId, Queue[IO, ServerMessage]]](Map.empty)
      refGameState             <- Ref.of[IO, GameState](GameState.empty)
      deck                     <- Deck()
      gameServerMessageService <- GameServerMessageService.apply(refOutMessageQueues).pure[IO]
      gameProcessingService <- GameProcessingService
        .apply(refGameState, gameServerMessageService, deck)
        .pure[IO]
      gameEngine                 <- GameEngineService.of(gameProcessingService)
      clientMessageServiceLogger <- Slf4jLogger.fromName[IO]("client-msg-service")
      clientMessageProcessingService <- ClientMessageProcessingService
        .apply(gameProcessingService, clientMessageServiceLogger)
        .pure[IO]
      pokerRoutesLogger <- Slf4jLogger.fromName[IO]("poker-routes")
      _ <- EmberServerBuilder
        .default[IO]
        .withHost(ipv4"127.0.0.1")
        .withPort(port"9000")
        .withHttpWebSocketApp(
          httpApp(
            refOutMessageQueues,
            gameProcessingService,
            clientMessageProcessingService,
            pokerRoutesLogger
          )
        )
        .withIdleTimeout(365.days)
        .build
        .both(gameEngine.startGameEngine.background)
        .useForever
    } yield ExitCode.Success
}
