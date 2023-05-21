package poker.services

import cats.effect.{IO, Ref}
import org.typelevel.log4cats.slf4j.Slf4jLogger
import poker.domain.game.{GameId, GamePhase, GameState, Outcome}
import poker.domain.player.{Decision, Hand, Player, PlayerId}
import poker.domain.card.Deck
import poker.services.GameProcessingService.{
  DecisionAccepted,
  DecisionsFinished,
  GameJoined,
  GameResolved,
  GameStarted,
  PlayerCount,
  PlayerLeaved,
  PlayersMoved,
  WaitForDecision
}

import java.util.UUID

sealed trait GameProcessingErrors
final case class WrongGamePhaseError(message: String) extends GameProcessingErrors

class GameProcessingService(
  gameState: Ref[IO, GameState],
  gameServerMessageService: GameServerMessageService,
  deck: Deck[IO]
) {

  /**
    * Should be called only once in the whole application
    */
  def firstEverWaitingForPlayers(): IO[Unit] =
    gameState.update { state =>
      state.gamePhase match {
        case _: GamePhase.WaitingForPlayers => state
        case _                              => state.copy(gamePhase = GamePhase.WaitingForPlayers(List.empty[Player]))
      }
    }

  def joinedPlayersBeforeGame(): IO[Either[WrongGamePhaseError, PlayerCount]] =
    gameState.get.map { state =>
      state.gamePhase match {
        case GamePhase.WaitingForPlayers(players) =>
          Right(PlayerCount(players.size))
        case other =>
          Left(
            WrongGamePhaseError(
              s"Joined players count should be checked only during ${GamePhase.WaitingForPlayers} phase, current game phase: $other)"
            )
          )
      }
    }

  def joinGame(player: Player): IO[Either[WrongGamePhaseError, GameJoined]] =
    for {
      logger <- Slf4jLogger.fromName[IO]("join-logger")
      result <- gameState.modify { state =>
        state.gamePhase match {
          case GamePhase.WaitingForPlayers(players) if !players.contains(player) =>
            val updatedState =
              state.copy(gamePhase = GamePhase.WaitingForPlayers(players :+ player))
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
      _ <- gameServerMessageService.playerJoined(result, playerId = player.id)
      _ <- logger.info(result.toString)
    } yield result

  def leaveGame(playerId: PlayerId): IO[PlayerLeaved] =
    for {
      result <- gameState.modify { state =>
        val playerLeaved = PlayerLeaved(playerId)
        state.gamePhase match {
          case GamePhase.WaitingForPlayers(joinedPlayers) =>
            val updatedState =
              state.copy(gamePhase =
                GamePhase.WaitingForPlayers(joinedPlayers.filterNot(p => p.id == playerId))
              )
            (updatedState, playerLeaved)
          case GamePhase.Started(gameId, playerHand, playing) =>
            val updatedState =
              state.copy(gamePhase =
                GamePhase.Started(gameId, playerHand, playing.filterNot(p => p.id == playerId))
              )
            (updatedState, playerLeaved)
          case GamePhase.WaitingForDecisions(
              gameId,
              playerHand,
              totalPlayers,
              willPlay,
              willFold
              ) =>
            val updatedState = state.copy(gamePhase = GamePhase.WaitingForDecisions(
              gameId,
              playerHand,
              totalPlayers.filterNot(p => p.id == playerId),
              willPlay.filterNot(p => p.id == playerId),
              willFold.filterNot(p => p.id == playerId)
            )
            )
            (updatedState, playerLeaved)
          case GamePhase.DecisionsAccepted(gameId, playerHand, played, folded) =>
            val updatedState = state.copy(gamePhase = GamePhase.DecisionsAccepted(
              gameId,
              playerHand,
              played.filterNot(p => p.id == playerId),
              folded.filterNot(p => p.id == playerId)
            )
            )
            (updatedState, playerLeaved)
          case GamePhase.Resolved(
              gameId,
              playerHand,
              dealerHand,
              outcome,
              playerWithGameOutcome,
              folded
              ) =>
            val updatedState = state.copy(gamePhase = GamePhase.Resolved(
              gameId,
              playerHand,
              dealerHand,
              outcome,
              playerWithGameOutcome.filterNot(p => p.id == playerId),
              folded.filterNot(p => p.id == playerId)
            )
            )
            (updatedState, playerLeaved)
        }
      }
    } yield result

  def startNewGame(): IO[Either[WrongGamePhaseError, GameStarted]] =
    for {
      _          <- deck.resetAndShuffle
      playerHand <- deck.drawCards(isPlayerHand = true)
      logger     <- Slf4jLogger.fromName[IO]("start-game-logger")
      result <- gameState.modify { state =>
        state.gamePhase match {
          case GamePhase.WaitingForPlayers(players) =>
            val gameId = GameId(UUID.randomUUID)

            val updatedState =
              state.copy(gamePhase = GamePhase.Started(gameId, playerHand, players))

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
      _ <- gameServerMessageService.gameStarted(result, playerHand)
      _ <- logger.info(result.toString)
    } yield result

  def waitForDecisions(): IO[Either[WrongGamePhaseError, WaitForDecision]] =
    gameState.modify { state =>
      state.gamePhase match {
        case GamePhase.Started(gameId, playerCards, players) =>
          val updatedState =
            state.copy(gamePhase =
              GamePhase.WaitingForDecisions(gameId, playerCards, players, Nil, Nil)
            )
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
    for {
      logger <- Slf4jLogger.fromName[IO]("accept-decision-logger")
      result <- gameState.modify { state =>
        state.gamePhase match {
          case GamePhase.WaitingForDecisions(gameId, hand, totalPlayers, willPlay, willFold)
              if !willPlay.exists(_.id == player.id) &&
                !willFold.exists(_.id == player.id) =>
            val updatedState = decision match {
              case Decision.Play =>
                state.copy(gamePhase = GamePhase
                  .WaitingForDecisions(gameId, hand, totalPlayers, willPlay :+ player, willFold)
                )
              case Decision.Fold =>
                state.copy(gamePhase = GamePhase
                  .WaitingForDecisions(gameId, hand, totalPlayers, willPlay, willFold :+ player)
                )
            }

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
      _ <- gameServerMessageService.decisionAccepted(player, decision, result)
      _ <- logger.info(result.toString)
    } yield result

  def decisionsFinished(): IO[Either[WrongGamePhaseError, DecisionsFinished]] =
    gameState.modify { state =>
      state.gamePhase match {
        case GamePhase.WaitingForDecisions(
            gameId,
            playerHand,
            totalPlayers,
            decidedToPlay,
            decidedToFold
            ) =>
          val autoFolded = totalPlayers.diff(decidedToPlay ++ decidedToFold)

          val totalFolded = decidedToFold ++ autoFolded
          val updatedState =
            state.copy(gamePhase =
              GamePhase.DecisionsAccepted(gameId, playerHand, decidedToPlay, totalFolded)
            )
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

  def resolveGame(): IO[Either[WrongGamePhaseError, GameResolved]] =
    for {
      logger     <- Slf4jLogger.fromName[IO]("resolve-game-logger")
      _          <- deck.resetAndShuffle
      dealerHand <- deck.drawCards(isPlayerHand = false)
      result <- gameState.modify { state =>
        state.gamePhase match {
          case GamePhase.DecisionsAccepted(gameId, playerHand, played, folded) =>
            val gameOutcome = HandComparisonUtil.compare(playerHand, dealerHand)
            val updatedState = state.copy(gamePhase =
              GamePhase.Resolved(gameId, playerHand, dealerHand, gameOutcome, played, folded)
            )
            (updatedState, Right(GameResolved(dealerHand, played, folded, gameOutcome)))
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
      _ <- gameServerMessageService.gameResolved(result)
      _ <- logger.info(result.toString)
    } yield result

  def waitNextGamePlayers(): IO[Either[WrongGamePhaseError, PlayersMoved]] =
    gameState.modify { state =>
      state.gamePhase match {
        case GamePhase.Resolved(_, _, _, _, played, folded) =>
          val playersForNextRound = played ++ folded
          val updatedState =
            state.copy(gamePhase = GamePhase.WaitingForPlayers(playersForNextRound))
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

object GameProcessingService {
  final case class DecisionAccepted()
  final case class DecisionsFinished()
  final case class GameJoined()

  final case class PlayerLeaved(playerd: PlayerId)

  final case class GameResolved(
    dealerHand: Hand,
    played: List[Player],
    folded: List[Player],
    gameOutcome: Outcome
  )
  final case class GameStarted(gameId: GameId)
  final case class PlayersMoved()
  final case class WaitForDecision()

  final case class PlayerCount(value: BigDecimal) extends AnyVal

  def apply(
    gameState: Ref[IO, GameState],
    gameServerMessageService: GameServerMessageService,
    deck: Deck[IO]
  ) = new GameProcessingService(gameState, gameServerMessageService, deck)
}
