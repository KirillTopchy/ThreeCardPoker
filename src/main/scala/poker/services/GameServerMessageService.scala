package poker.services

import cats.effect.std.Queue
import cats.effect.{IO, Ref}
import cats.implicits.toFoldableOps
import io.circe.syntax.EncoderOps
import poker.domain.player.{Decision, Hand, Player, PlayerId}
import poker.server.JsonCodec.{handCodec, outcomeCodec}
import poker.server.ServerMessage
import poker.services.GameProcessingService.{
  BetAccepted,
  BetsFinished,
  DecisionAccepted,
  GameJoined,
  GameResolved,
  GameStarted,
  WaitForBet,
  WaitForDecision
}

class GameServerMessageService(
  refOutMessageQueues: Ref[IO, Map[PlayerId, Queue[IO, ServerMessage]]]
) {

  def gameStarted(result: Either[WrongGamePhaseError, GameStarted]): IO[Unit] =
    result.fold(
      _ => sendMessageToAllPlayers(s"Game already started"),
      gameStarted => sendMessageToAllPlayers(s"Game with id: ${gameStarted.gameId.value} Started")
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

  def waitingForBetsStarted(result: Either[WrongGamePhaseError, WaitForBet]): IO[Unit] =
    result.fold(
      _ => sendMessageToAllPlayers(s"Bets phase is over"),
      _ => sendMessageToAllPlayers(s"Place your bets!!!")
    )

  def waitingForDecisionsStarted(
    result: Either[WrongGamePhaseError, BetsFinished],
    playerHand: Hand
  ): IO[Unit] =
    result.fold(
      _ => sendMessageToAllPlayers(s"Decision phase is over"),
      _ =>
        sendMessageToAllPlayers(playerHand.asJson.noSpaces) *>
          sendMessageToAllPlayers(s"Make your decisions!!!")
    )

  def betAccepted(
    player: Player,
    bet: Double,
    result: Either[WrongGamePhaseError, BetAccepted]
  ): IO[Unit] =
    result.fold(
      _ =>
        sendMessageToSpecificPlayer(
          s"Players with id: ${player.id.value} Your Bet - $bet Rejected",
          player.id
        ),
      _ =>
        sendMessageToSpecificPlayer(
          s"Player with id: ${player.id.value} Your Bet - $bet Accepted",
          player.id
        )
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
    result.fold(
      _ => {
        val errorMessage = "An error occurred. Your bets will be refunded shortly."
        sendMessageToAllPlayers(errorMessage)
      },
      gameResolved => {
        val dealerHand    = gameResolved.dealerHand
        val outcome       = gameResolved.gameOutcome
        val playerWhoBet  = gameResolved.playersWhoBet
        val played        = gameResolved.played
        val folded        = gameResolved.folded
        val playedMessage = outcome.asJson.noSpaces
        val foldedMessage = s"Game outcome: $outcome, but You folded, so its lose anyway ;)."
        sendMessageToAllPlayers(dealerHand.asJson.noSpaces) *>
          sendMessageToAllPlayers(outcome.asJson.noSpaces) *>
          played.traverse_(
            player =>
              sendMessageToSpecificPlayer(
                s"Your total bet: ${player.bet}, new balance: ${player.balance}",
                player.id
              )
          ) *>
          folded.traverse_(
            player =>
              sendMessageToSpecificPlayer(
                "sYour total bet: ${player.bet}, new balance: ${player.balance}",
                player.id
              )
          ) *>
          //folded.traverse_(player => sendMessageToSpecificPlayer(foldedMessage, player.id)) *>
          sendMessageToAllPlayers("Next game will start shortly")
      }
    )
}
object GameServerMessageService {
  def apply(refOutMessageQueues: Ref[IO, Map[PlayerId, Queue[IO, ServerMessage]]]) =
    new GameServerMessageService(refOutMessageQueues)
}
