package poker.services

import cats.effect.{IO, Ref}
import cats.effect.std.Queue
import cats.implicits.toFoldableOps
import poker.domain.player.{Decision, PlayerId}
import poker.server.{ClientMessage, ServerMessage}
import poker.services.GameProcessingService.GameJoined

object GameServerMessageService {
  def playerJoined(
                    refMessageQueues: Ref[IO, Map[PlayerId, (Queue[IO, ServerMessage], Queue[IO, ClientMessage])]],
                    result: Either[WrongGamePhaseError, GameJoined],
                    playerId: PlayerId
                  ): IO[Unit] = {
    def sendMessage(message: String, targetPlayerId: PlayerId): IO[Unit] =
      for {
        messageQueues <- refMessageQueues.get
        _ <- messageQueues.get(targetPlayerId).traverse_ {
          case (serverQueue, _) if targetPlayerId == playerId =>
            serverQueue.offer(ServerMessage.Message(message))
        }
      } yield ()

    result.fold(
      _ => sendMessage("Cannot join game now, please wait.", playerId),
      _ => sendMessage("Game Joined", playerId)
    )
  }


  def decisionAccepted(
                    refMessageQueues: Ref[IO, Map[PlayerId, (Queue[IO, ServerMessage], Queue[IO, ClientMessage])]],
                    decision: Decision
                  ): IO[Unit] =
    for {
      messageQueues <- refMessageQueues.get
      _ <- messageQueues.keys.toList.traverse_ { playerId =>
        val (serverQueue, _) = messageQueues(playerId)
        serverQueue.offer(ServerMessage.Message(s"Players ${playerId.value} decision - $decision accepted"))
      }
    } yield ()
}