package poker.services

import cats.effect.{IO, Ref}
import poker.domain.game.{GameId, GamePhase, GameState, Outcome}
import poker.domain.player.{Decision, Hand, Player, PlayerState}

import java.util.UUID

final case class DecisionAccepted()
final case class DecisionsFinished()
final case class GameJoined()
final case class GameResolved ()
final case class GameStarted (gameId : GameId)
final case class PlayersMoved ()
final case class WaitForDecision ()

sealed trait GameProcessingErrors
final case class WrongGamePhaseError(message: String) extends GameProcessingErrors

case class GameProcessingService(gameState: Ref[IO, GameState]) {

  def joinGame(player: Player): IO[Either[WrongGamePhaseError, GameJoined]] =
    gameState.modify { state =>
      state.gamePhase match {
        case GamePhase.WaitingForPlayers =>
          val updatedState = state.copy(players = state.players + ((player, PlayerState.Playing)))
          (updatedState, Right(GameJoined()))
        case phase =>
          (
            state,
            Left(
              WrongGamePhaseError(
                s"Cannot join game when game phase is not waiting for players (current game phase: $phase)"
              )
            )
          )
      }
    }

  def startNewGame(playerCards: Hand): IO[Either[WrongGamePhaseError, GameStarted]] =
    gameState.modify { state =>
      state.gamePhase match {
        case GamePhase.WaitingForPlayers =>
          val gameId = GameId(UUID.randomUUID)
          val updatedState =
            state.copy(gamePhase = GamePhase.Started(gameId, playerCards, state.players.keys.toList)
            )
          (updatedState, Right(GameStarted(gameId)))
        case phase =>
          (
            state,
            Left(
              WrongGamePhaseError(
                s"Cannot start game when game phase is not waiting for players (current game phase: $phase)"
              )
            )
          )
      }
    }

  def waitForDecisions(): IO[Either[WrongGamePhaseError, WaitForDecision]] =
    gameState.modify { state =>
      state.gamePhase match {
        case GamePhase.Started(gameId, playerCards, players) =>
          val updatedState =
            state.copy(gamePhase = GamePhase.WaitingForDecisions(gameId, playerCards, players))
          (updatedState, Right(WaitForDecision()))
        case phase =>
          (
            state,
            Left(
              WrongGamePhaseError(
                s"Cannot wait for decisions when game phase is not started (current game phase: $phase)"
              )
            )
          )
      }
    }

  def acceptDecision(
                      player: Player,
                      decision: Decision
                    ): IO[Either[WrongGamePhaseError, DecisionAccepted]] =
    gameState.modify { state =>
      state.gamePhase match {
        case GamePhase.WaitingForDecisions(_, _, _) =>
          val updatedState =
            state.copy(players = state.players + ((player, PlayerState.DecisionMade(decision))))
          (updatedState, Right(DecisionAccepted()))
        case phase =>
          (
            state,
            Left(
              WrongGamePhaseError(
                s"Cannot accept decision when game phase is not waiting for decisions (current game phase: $phase)"
              )
            )
          )
      }
    }

  def decisionsFinished(): IO[Either[WrongGamePhaseError, DecisionsFinished]] =
    gameState.modify { state =>
      state.gamePhase match {
        case GamePhase.WaitingForDecisions(gameId, playerCards, _) =>
          val played = state.players
            .filter { case (_, state) => state == PlayerState.DecisionMade(Decision.Play) }
            .keys
            .toList
          val folded = state.players.keys.toList.diff(played)
          val updatedState =
            state.copy(gamePhase = GamePhase.DecisionsAccepted(gameId, playerCards, played, folded))
          (updatedState, Right(DecisionsFinished()))
        case phase =>
          (
            state,
            Left(
              WrongGamePhaseError(
                s"Cannot finish decisions game when game phase is not waiting for decisions (current game phase: $phase)"
              )
            )
          )
      }
    }

  def resolveGame(dealerHand: Hand): IO[Either[WrongGamePhaseError, GameResolved]] =
    gameState.modify { state =>
      state.gamePhase match {
        case GamePhase.DecisionsAccepted(gameId, playerCards, _, _) =>
          val gameOutcome = HandComparisonUtil.compare(playerCards, dealerHand)

          val playerOutcomes: Map[Player, PlayerState] = state.players.map {
            case (p, playerState) =>
              val playerOutcomeState = playerState match {
                case PlayerState.DecisionMade(decision) =>
                  PlayerState.OutcomeGiven(gameOutcome, decision)
                case _ =>
                  PlayerState.OutcomeGiven(Outcome.Folded, Decision.Fold)
              }
              (p, playerOutcomeState)
          }

          val updatedState = state.copy(
            players = playerOutcomes,
            gamePhase = GamePhase.Resolved(gameId, playerCards, dealerHand, gameOutcome)
          )
          (updatedState, Right(GameResolved()))
        case phase =>
          (
            state,
            Left(
              WrongGamePhaseError(
                s"Cannot resolve the game when game phase is not decisions accepted (current game phase: $phase)"
              )
            )
          )
      }
    }

  def movePlayersToNextGame(): IO[Either[WrongGamePhaseError, PlayersMoved]] =
    gameState.modify { state =>
      state.gamePhase match {
        case GamePhase.Resolved(_, _, _, _) =>
          val updatedState = state.copy(
            gamePhase = GamePhase.WaitingForPlayers,
            players = state.players.view.mapValues(_ => PlayerState.Playing).toMap
          )
          (updatedState, Right(PlayersMoved()))
        case phase =>
          (
            state,
            Left(
              WrongGamePhaseError(
                s"Cannot move players to the next game when game phase is not resolved (current game phase: $phase)"
              )
            )
          )
      }
    }
}
