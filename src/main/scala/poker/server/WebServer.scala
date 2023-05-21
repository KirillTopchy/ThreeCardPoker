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
import poker.services.{GameEngineService, GameProcessingService, GameServerMessageService}

import scala.concurrent.duration.DurationInt

object WebServer extends IOApp {
  private def httpApp(
                       refMessageQueues: Ref[IO, Map[PlayerId, (Queue[IO, ServerMessage], Queue[IO, ClientMessage])]],
                       refGameState: Ref[IO, GameState],
                       deck: Deck[IO],
                       gameProcessingService: GameProcessingService,
                       logger: SelfAwareStructuredLogger[IO]
                     )(wsb: WebSocketBuilder2[IO]): HttpApp[IO] = {
    val routes = PokerRoutes(refMessageQueues, refGameState, deck, gameProcessingService, logger)
    routes.createRoute(wsb)
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    for {
      refMessageQueues <- Ref
        .of[IO, Map[PlayerId, (Queue[IO, ServerMessage], Queue[IO, ClientMessage])]](Map.empty)
      refGameState          <- Ref.of[IO, GameState](GameState.empty)
      deck                  <- Deck()
      gameProcessingService <- GameProcessingService.apply(refGameState, refMessageQueues, deck).pure[IO]
      gameEngine            <- GameEngineService.of(gameProcessingService)
      logger   <- Slf4jLogger.fromName[IO]("poker-routes")
      _ <- EmberServerBuilder
        .default[IO]
        .withHost(ipv4"127.0.0.1")
        .withPort(port"9000")
        .withHttpWebSocketApp(httpApp(refMessageQueues, refGameState, deck, gameProcessingService, logger))
        .withIdleTimeout(365.days)
        .build
        .both(gameEngine.startGameEngine.background)
        .useForever
    } yield ExitCode.Success
}