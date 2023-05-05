package poker.server

import cats.effect.std.Queue
import cats.effect.{ExitCode, IO, IOApp, Ref}
import com.comcast.ip4s._
import org.http4s.HttpApp
import org.http4s.ember.server._
import org.http4s.server.websocket.WebSocketBuilder2
import poker.domain.card.Deck

import java.util.UUID

object WebServer extends IOApp {
  private def httpApp(
                       refMessageQueues: Ref[IO, Map[UUID, (Queue[IO, ServerMessage], Queue[IO, ClientMessage])]],
                       deck: Deck[IO]
                     )(wsb: WebSocketBuilder2[IO]): HttpApp[IO] = {
    PokerRoutes.createRoute(wsb, refMessageQueues, deck)
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    for {
      refMessageQueues <- Ref.of[IO, Map[UUID, (Queue[IO, ServerMessage], Queue[IO, ClientMessage])]](Map.empty)
      deck <- Deck()
      _ <- EmberServerBuilder
        .default[IO]
        .withHost(ipv4"127.0.0.1")
        .withPort(port"9000")
        .withHttpWebSocketApp(httpApp(refMessageQueues, deck))
        .build
        .useForever
    } yield ExitCode.Success
}
