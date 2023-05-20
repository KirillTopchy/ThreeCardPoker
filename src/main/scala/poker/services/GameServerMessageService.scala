package poker.services

import cats.effect.{IO, Ref}
import cats.effect.std.Queue
import cats.implicits.toFoldableOps
import poker.domain.player.PlayerId
import poker.server.{ClientMessage, ServerMessage}

object GameServerMessageService {
  def test(
            refMessageQueues: Ref[IO, Map[PlayerId, (Queue[IO, ServerMessage], Queue[IO, ClientMessage])]]
          ): IO[Unit] =
    for {
      messageQueues <- refMessageQueues.get
      _ <- messageQueues.keys.toList.traverse_ { playerId =>
        val (serverQueue, _) = messageQueues(playerId)
        serverQueue.offer(ServerMessage.Message(s"JOINED $playerId"))
      }
    } yield ()
}