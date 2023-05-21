package poker.server

import cats.effect.kernel.Resource.ExitCase
import cats.effect.std.Queue
import cats.effect.{IO, Ref}
import fs2.{Pipe, Stream}
import io.circe.jawn
import io.circe.syntax._
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text
import org.typelevel.log4cats.SelfAwareStructuredLogger
import poker.domain.player.PlayerId
import poker.server.JsonCodec._
import poker.services.{ClientMessageProcessingService, GameProcessingService}

import java.util.UUID

class PokerRoutes(
  refOutMessageQueues: Ref[IO, Map[PlayerId, Queue[IO, ServerMessage]]],
  gameProcessingService: GameProcessingService,
  clientMessageProcessingService: ClientMessageProcessingService,
  logger: SelfAwareStructuredLogger[IO]
) {

  def createRoute(wsb: WebSocketBuilder2[IO]): HttpRoutes[IO] = HttpRoutes.of[IO] {

    case GET -> Root / "poker" =>
      val pokerPipe: Pipe[IO, ServerMessage, WebSocketFrame] =
        _.flatMap { message =>
          Stream(WebSocketFrame.Text(message.asJson.noSpaces))
        }

      for {
        queueOut <- Queue.unbounded[IO, ServerMessage]
        playerId = PlayerId(UUID.randomUUID())
        _ <- refOutMessageQueues.update(_.updated(playerId, queueOut))
        response <- wsb.build(
          // Sink, where the incoming WebSocket messages from the client are pushed to.
          receive = _.evalMap {
            case text: Text =>
              jawn.decode[ClientMessage](text.str) match {
                case Left(value) =>
                  logger.info(s"Failed to parse message: $value")
                case Right(clientMessage) =>
                  for {
                    _ <- clientMessageProcessingService
                      .processSingleMessage(playerId, clientMessage)
                  } yield ()
              }
            case _ => IO.unit
          },
          // Outgoing stream of WebSocket messages to send to the client.
          send = Stream.repeatEval(queueOut.take).through(pokerPipe).onFinalizeCaseWeak { error =>
            val logTermination: IO[Unit] = error match {
              case ExitCase.Errored(e) =>
                logger.info(s"Outgoing stream exited with error: ${e.getMessage}")
              case other => logger.info(s"Outgoing stream exited with $other")
            }
            gameProcessingService.leaveGame(playerId) *> logTermination

          }
        )
      } yield response
  }
}

object PokerRoutes {
  def apply(
    refOutMessageQueues: Ref[IO, Map[PlayerId, Queue[IO, ServerMessage]]],
    gameProcessingService: GameProcessingService,
    clientMessageProcessingService: ClientMessageProcessingService,
    logger: SelfAwareStructuredLogger[IO]
  ) =
    new PokerRoutes(
      refOutMessageQueues,
      gameProcessingService,
      clientMessageProcessingService,
      logger
    )
}
