package poker.services

import cats.effect.{IO, Ref}
import cats.effect.std.Queue
import cats.implicits.toFoldableOps
import io.circe.syntax.EncoderOps
import poker.domain.player.{Decision, Hand, Player, PlayerId}
import poker.server.JsonCodec.{handCodec, outcomeCodec}
import poker.server.{ClientMessage, ServerMessage}
import poker.services.GameProcessingService.{
  DecisionAccepted,
  GameJoined,
  GameResolved,
  GameStarted
}

object GameServerMessageService {

  def gameStarted(
    refMessageQueues: Ref[IO, Map[PlayerId, (Queue[IO, ServerMessage], Queue[IO, ClientMessage])]],
    result: Either[WrongGamePhaseError, GameStarted],
    playerHand: Hand
  ): IO[Unit] =
    result.fold(
      _ => sendMessageToAllPlayers(s"Game already started", refMessageQueues),
      gameStarted => {
        val gameIdMessage = s"Game with ID: ${gameStarted.gameId} Started"
        sendMessageToAllPlayers(gameIdMessage, refMessageQueues) *>
        sendMessageToAllPlayers(playerHand.asJson.noSpaces, refMessageQueues)
      }
    )

  private def sendMessageToAllPlayers(
    message: String,
    refMessageQueues: Ref[IO, Map[PlayerId, (Queue[IO, ServerMessage], Queue[IO, ClientMessage])]]
  ): IO[Unit] =
    for {
      messageQueues <- refMessageQueues.get
      _ <- messageQueues.values.toList.traverse_ {
        case (serverQueue, _) =>
          serverQueue.offer(ServerMessage.Message(message))
      }
    } yield ()

  private def sendMessageToSpecificPlayer(
    message: String,
    playerId: PlayerId,
    refMessageQueues: Ref[IO, Map[PlayerId, (Queue[IO, ServerMessage], Queue[IO, ClientMessage])]]
  ): IO[Unit] =
    for {
      messageQueues <- refMessageQueues.get
      _ <- messageQueues.get(playerId).traverse_ {
        case (serverQueue, _) =>
          serverQueue.offer(ServerMessage.Message(message))
      }
    } yield ()

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
      _ =>
        sendMessageToSpecificPlayer(
          s"Player with ID: ${playerId.value} cannot Join the Game",
          playerId,
          refMessageQueues
        ),
      _ =>
        sendMessageToSpecificPlayer(
          s"Player with ID: ${playerId.value} Joined the Game",
          playerId,
          refMessageQueues
        )
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
        sendMessageToSpecificPlayer(
          s"Players with id: ${player.id.value} Decision - $decision Rejected",
          player.id,
          refMessageQueues
        ),
      _ =>
        sendMessageToSpecificPlayer(
          s"Player with id: ${player.id.value} Decision - $decision Accepted",
          player.id,
          refMessageQueues
        )
    )
  }

  def gameResolved(
    refMessageQueues: Ref[IO, Map[PlayerId, (Queue[IO, ServerMessage], Queue[IO, ClientMessage])]],
    result: Either[WrongGamePhaseError, GameResolved]
  ): IO[Unit] =
    result.fold(
      _ => {
        val errorMessage = "An error occurred. Your bets will be refunded shortly."
        sendMessageToAllPlayers(errorMessage, refMessageQueues)
      },
      gameResolved => {
        val dealerHand = gameResolved.dealerHand
        val outcome       = gameResolved.gameOutcome
        val played        = gameResolved.played
        val folded        = gameResolved.folded
        val playedMessage = outcome.asJson.noSpaces
        val foldedMessage = s"Game outcome: $outcome, but you auto-folded so you lose anyway ;)"
        sendMessageToAllPlayers(dealerHand.asJson.noSpaces, refMessageQueues) *>
          played.traverse_(
            player => sendMessageToSpecificPlayer(playedMessage, player.id, refMessageQueues)
          ) *>
          folded.traverse_(
            player => sendMessageToSpecificPlayer(foldedMessage, player.id, refMessageQueues)
          )
      }
    )
}
