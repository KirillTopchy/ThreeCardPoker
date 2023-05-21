package poker.services

import cats.effect.std.Queue
import cats.effect.{IO, Ref}
import cats.implicits.toFoldableOps
import io.circe.syntax.EncoderOps
import poker.domain.player.{Decision, Hand, Player, PlayerId}
import poker.server.JsonCodec.{handCodec, outcomeCodec}
import poker.server.ServerMessage
import poker.services.GameProcessingService.{
  DecisionAccepted,
  GameJoined,
  GameResolved,
  GameStarted
}

class GameServerMessageService(
  refOutMessageQueues: Ref[IO, Map[PlayerId, Queue[IO, ServerMessage]]]
) {

  def gameStarted(result: Either[WrongGamePhaseError, GameStarted], playerHand: Hand): IO[Unit] =
    result.fold(
      _ => sendMessageToAllPlayers(s"Game already started"),
      gameStarted =>
        sendMessageToAllPlayers(s"Game with id: ${gameStarted.gameId.value} Started") *>
          sendMessageToAllPlayers(playerHand.asJson.noSpaces)
    )

  private def sendMessageToAllPlayers(message: String): IO[Unit] =
    for {
      messageQueues <- refOutMessageQueues.get
      _ <- messageQueues.values.toList.traverse_ { serverQueue =>
        serverQueue.offer(ServerMessage.Message(message))
      }
    } yield ()

  private def sendMessageToSpecificPlayer(message: String, playerId: PlayerId): IO[Unit] =
    for {
      messageQueues <- refOutMessageQueues.get
      _ <- messageQueues.get(playerId).traverse_ { serverQueue =>
        serverQueue.offer(ServerMessage.Message(message))
      }
    } yield ()

  def playerJoined(result: Either[WrongGamePhaseError, GameJoined], playerId: PlayerId): IO[Unit] =
    result.fold(
      _ =>
        sendMessageToSpecificPlayer(
          s"Player with id: ${playerId.value} cannot Join the Game",
          playerId
        ),
      _ =>
        sendMessageToSpecificPlayer(s"Player with id: ${playerId.value} Joined the Game", playerId)
    )

  def decisionAccepted(
    player: Player,
    decision: Decision,
    result: Either[WrongGamePhaseError, DecisionAccepted]
  ): IO[Unit] =
    result.fold(
      _ =>
        sendMessageToSpecificPlayer(
          s"Players with id: ${player.id.value} Decision - $decision Rejected",
          player.id
        ),
      _ =>
        sendMessageToSpecificPlayer(
          s"Player with id: ${player.id.value} Decision - $decision Accepted",
          player.id
        )
    )

  def gameResolved(result: Either[WrongGamePhaseError, GameResolved]): IO[Unit] =
    result.fold(_ => {
      val errorMessage = "An error occurred. Your bets will be refunded shortly."
      sendMessageToAllPlayers(errorMessage)
    }, gameResolved => {
      val dealerHand    = gameResolved.dealerHand
      val outcome       = gameResolved.gameOutcome
      val played        = gameResolved.played
      val folded        = gameResolved.folded
      val playedMessage = outcome.asJson.noSpaces
      val foldedMessage = s"Game outcome: $outcome, but You folded, so its lose anyway ;)."
      sendMessageToAllPlayers(dealerHand.asJson.noSpaces) *>
        played.traverse_(player => sendMessageToSpecificPlayer(playedMessage, player.id)) *>
        folded.traverse_(player => sendMessageToSpecificPlayer(foldedMessage, player.id))
    })
}
object GameServerMessageService {
  def apply(refOutMessageQueues: Ref[IO, Map[PlayerId, Queue[IO, ServerMessage]]]) =
    new GameServerMessageService(refOutMessageQueues)
}
