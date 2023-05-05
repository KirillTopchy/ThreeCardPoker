package poker.server

import cats.effect.std.Queue
import cats.effect.{ExitCode, IO, IOApp, Ref}
import com.comcast.ip4s._
import fs2.{Pipe, Stream}
import org.http4s.ember.server._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text
import org.http4s.{HttpRoutes, _}
import io.circe.syntax._
import io.circe.parser.decode
import poker.domain.card.Deck

import java.util.UUID

object WebServer extends IOApp {

  import poker.server.JsonCodec._

  private def pokerRoute(
                          wsb: WebSocketBuilder2[IO],
                          refMessageQueues: Ref[IO, Map[UUID, (Queue[IO, ServerMessage], Queue[IO, ClientMessage])]],
                          deck: Deck[IO]
                        ): HttpRoutes[IO] = HttpRoutes.of[IO] {


    case GET -> Root / "poker" =>

      val pokerPipe: Pipe[IO, ClientMessage, WebSocketFrame] =
        _.flatMap { message =>
          Stream(WebSocketFrame.Text(message.asJson.noSpaces))
        }

      for {
        queueIn <- Queue.unbounded[IO, ServerMessage]
        queueOut <- Queue.unbounded[IO, ClientMessage]

        _ <- refMessageQueues.update(_.updated(UUID.randomUUID(), (queueIn, queueOut)))

        response <- wsb.build(
          // Sink, where the incoming WebSocket messages from the client are pushed to.
          receive = _.evalMap {
            case text: Text => decode[ServerMessage](text.str) match {
              case Left(value) => ??? // add logger
              case Right(value) => queueIn.offer(value)
            }
            case _ => IO.unit
          },
          // Outgoing stream of WebSocket messages to send to the client.
          send = Stream.repeatEval(queueOut.take).through(pokerPipe),
        )
      } yield response
  }

  private def httpApp(
                       refMessageQueues: Ref[IO, Map[UUID, (Queue[IO, ServerMessage], Queue[IO, ClientMessage])]],
                       deck: Deck[IO]
                     )(wsb: WebSocketBuilder2[IO]): HttpApp[IO] = {
    pokerRoute(wsb, refMessageQueues, deck)
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    for {
      refMessageQueues <- Ref.of[IO, Map[UUID, (Queue[IO, ServerMessage], Queue[IO, ClientMessage])]](Map.empty)
      deck             <- Deck()
      _ <- EmberServerBuilder
        .default[IO]
        .withHost(ipv4"127.0.0.1")
        .withPort(port"9000")
        .withHttpWebSocketApp(httpApp(refMessageQueues, deck))
        .build
        .useForever
    } yield ExitCode.Success
}
