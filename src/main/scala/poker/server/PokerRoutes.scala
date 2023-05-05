package poker.server

import cats.effect.std.Queue
import cats.effect.{IO, Ref}
import com.typesafe.scalalogging.LazyLogging
import fs2.{Pipe, Stream}
import io.circe.jawn
import io.circe.syntax._
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text
import poker.domain.card.Deck
import poker.server.JsonCodec._

import java.util.UUID

object PokerRoutes extends LazyLogging {

  def createRoute(
                   wsb: WebSocketBuilder2[IO],
                   refMessageQueues: Ref[IO, Map[UUID, (Queue[IO, ServerMessage], Queue[IO, ClientMessage])]],
                   deck: Deck[IO]
                 ): HttpRoutes[IO] = HttpRoutes.of[IO] {

    case GET -> Root / "poker" =>

      val pokerPipe: Pipe[IO, ServerMessage, WebSocketFrame] =
        _.flatMap { message =>
          Stream(WebSocketFrame.Text(message.asJson.noSpaces))
        }

      for {
        queueIn <- Queue.unbounded[IO, ClientMessage]
        queueOut <- Queue.unbounded[IO, ServerMessage]

        _ <- refMessageQueues.update(_.updated(UUID.randomUUID(), (queueOut, queueIn)))

        response <- wsb.build(
          // Sink, where the incoming WebSocket messages from the client are pushed to.
          receive = _.evalMap {
            case text: Text => jawn.decode[ClientMessage](text.str) match {
              case Left(value) =>
                IO(logger.info(s"Failed to parse message: $value"))
              case Right(clientMessage) =>
                val serverMessage = TestMessage("AAA")
                queueIn.tryOffer(clientMessage).flatMap { sent =>
                  if (sent) {
                    queueOut.offer(serverMessage) *>
                      IO(logger.info(s"Message from client '$clientMessage' added to queueIn and queueOut."))
                  } else {
                    IO(logger.info(s"Failed to add message from client '$clientMessage' to queueIn."))
                  }
                }
            }
            case _ => IO.unit
          },

          // Outgoing stream of WebSocket messages to send to the client.
          send = Stream.eval(queueOut.take).through(pokerPipe)
        )
      } yield response
  }
}
