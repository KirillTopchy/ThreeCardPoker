package poker.server

import cats.effect.kernel.Resource.ExitCase
import cats.effect.std.Queue
import cats.effect.{IO, Ref}
import cats.implicits.toFoldableOps
import com.typesafe.scalalogging.LazyLogging
import fs2.{Pipe, Stream}
import io.circe.jawn
import io.circe.syntax._
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import poker.domain.card.Deck
import poker.domain.game.GameState
import poker.domain.player.{Decision, Hand, Player, PlayerId}
import poker.server.ClientMessage.{Join, PlayerDecision}
import poker.server.JsonCodec._
import poker.services.GameProcessingService

import java.util.UUID

case class PokerRoutes(
  refMessageQueues: Ref[IO, Map[PlayerId, (Queue[IO, ServerMessage], Queue[IO, ClientMessage])]],
  refGameState: Ref[IO, GameState],
  deck: Deck[IO],
  gameProcessingService: GameProcessingService,
  logger: SelfAwareStructuredLogger[IO]
) {

  private def process: IO[Unit] =
    for {
      messageQueues <- refMessageQueues.get
      _ <- messageQueues.keys.toList.traverse_ { playerId =>
        val (_, clientQueue) = messageQueues(playerId)
        clientQueue.take.flatMap {
          case Join() => gameProcessingService.joinGame(Player(playerId))
          case PlayerDecision(decision) =>
            gameProcessingService.acceptDecision(Player(playerId), decision)
        }
      }
    } yield ()

  def createRoute(wsb: WebSocketBuilder2[IO]): HttpRoutes[IO] = HttpRoutes.of[IO] {

    case GET -> Root / "poker" =>
      val pokerPipe: Pipe[IO, ServerMessage, WebSocketFrame] =
        _.flatMap { message =>
          Stream(WebSocketFrame.Text(message.asJson.noSpaces))
        }

      for {
        queueIn  <- Queue.unbounded[IO, ClientMessage]
        queueOut <- Queue.unbounded[IO, ServerMessage]

        _ <- refMessageQueues.update(_.updated(PlayerId(UUID.randomUUID()), (queueOut, queueIn)))

        response <- wsb.build(
          // Sink, where the incoming WebSocket messages from the client are pushed to.
          receive = _.evalMap {
            case text: Text =>
              jawn.decode[ClientMessage](text.str) match {
                case Left(value) =>
                  logger.info(s"Failed to parse message: $value")
                case Right(clientMessage) =>
                  for {
                    _ <- queueIn.offer(clientMessage)
                    _ <- process
                  } yield ()
              }
            case _ => IO.unit
          },
          // Outgoing stream of WebSocket messages to send to the client.
          send = Stream.repeatEval(queueOut.take).through(pokerPipe).onFinalizeCaseWeak {
            case ExitCase.Errored(e) =>
              logger.info(s"Outgoing stream exited with error: ${e.getMessage}")
            case other => logger.info(s"Outgoing stream exited with $other")
          }
        )
      } yield response
  }
}
