package poker.services

import cats.effect.{IO, Ref}
import cats.effect.std.Queue
import cats.implicits.toFoldableOps
import io.circe.syntax.EncoderOps
import poker.domain.game.GameState
import poker.domain.player.{Decision, Hand, Player, PlayerId}
import poker.server.JsonCodec.handCodec
import poker.server.{ClientMessage, ServerMessage}
import poker.services.GameProcessingService.{DecisionAccepted, GameJoined, GameStarted}

object GameServerMessageService {

  def gameStarted(
    refMessageQueues: Ref[IO, Map[PlayerId, (Queue[IO, ServerMessage], Queue[IO, ClientMessage])]],
    result: Either[WrongGamePhaseError, GameStarted],
    playerHand : Hand
  ): IO[Unit] = {
    def sendMessage(message: String): IO[Unit] =
      for {
        messageQueues <- refMessageQueues.get
        _ <- messageQueues.values.toList.traverse_ {
          case (serverQueue, _) =>
            serverQueue.offer(ServerMessage.Message(message))
        }
      } yield ()


    result.fold(
      _ => sendMessage(s"Game already started"),
      _ => sendMessage(playerHand.asJson.noSpaces)
    )
  }

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
      _ => sendMessage(s"Player with id: ${playerId.value} cannot Join the Game", playerId),
      _ => sendMessage(s"Player with id: ${playerId.value} Joined the Game", playerId)
    )
  }

  def decisionAccepted(
    refMessageQueues: Ref[IO, Map[PlayerId, (Queue[IO, ServerMessage], Queue[IO, ClientMessage])]],
    player: Player,
    decision: Decision,
    result: Either[WrongGamePhaseError, DecisionAccepted]
  ): IO[Unit] = {
    def sendMessage(message: String, targetPlayerId: PlayerId): IO[Unit] =
      for {
        messageQueues <- refMessageQueues.get
        _ <- messageQueues.get(targetPlayerId).traverse_ {
          case (serverQueue, _) if targetPlayerId == player.id =>
            serverQueue.offer(ServerMessage.Message(message))
        }
      } yield ()

    result.fold(
      _ =>
        sendMessage(
          s"Players with id: ${player.id.value} Decision - $decision Rejected",
          player.id
        ),
      _ =>
        sendMessage(s"Player with id: ${player.id.value} Decision - $decision Accepted", player.id)
    )
  }
}