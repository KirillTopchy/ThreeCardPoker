package poker.services

import cats.effect.{IO, Ref}
import org.typelevel.log4cats.slf4j.Slf4jLogger
import poker.domain.game.{GameId, GamePhase, GameState, Outcome}
import poker.domain.player.{Decision, Hand, Player, PlayerId}
import poker.domain.card.Deck
import poker.services.GameProcessingService.{
  BetAccepted,
  BetsFinished,
  DecisionAccepted,
  DecisionsFinished,
  GameJoined,
  GameResolved,
  GameStarted,
  PlayerCount,
  PlayerLeaved,
  PlayersMoved,
  WaitForBet,
  WaitForDecision
}

import java.util.UUID
import scala.collection.convert.ImplicitConversions.{
  `collection asJava`,
  `map AsJavaMap`,
  `seq AsJavaList`
}

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
          case GamePhase.Started(gameId, playing) =>
            val updatedState =
              state.copy(gamePhase =
                GamePhase.Started(gameId, playing.filterNot(p => p.id == playerId))
              )
            (updatedState, playerLeaved)
          case GamePhase.WaitingForBets(gameId, totalPlayers, playersBets) =>
            val updatedState = state.copy(gamePhase = GamePhase.WaitingForBets(
              gameId,
              totalPlayers.filterNot(p => p.id == playerId),
              playersBets.filterNot(_._1 == playerId)
            )
            )
            (updatedState, playerLeaved)
          case GamePhase.BetsAccepted(gameId, totalPlayers, playersBets, hand) =>
            val updatedState = state.copy(gamePhase = GamePhase.BetsAccepted(
              gameId,
              totalPlayers.filterNot(p => p.id == playerId),
              playersBets.filterNot(_._1 == playerId),
              hand
            )
            )
            (updatedState, playerLeaved)
          case GamePhase.WaitingForDecisions(
              gameId,
              totalPlayers,
              playersBets,
              willPlay,
              willFold,
              playerHand
              ) =>
            val updatedState = state.copy(gamePhase = GamePhase.WaitingForDecisions(
              gameId,
              totalPlayers.filterNot(p => p.id == playerId),
              playersBets.filterNot(_._1 == playerId),
              willPlay.filterNot(p => p.id == playerId),
              willFold.filterNot(p => p.id == playerId),
              playerHand
            )
            )
            (updatedState, playerLeaved)
          case GamePhase.DecisionsAccepted(
              gameId,
              totalPlayers,
              playersBets,
              played,
              folded,
              playerHand
              ) =>
            val updatedState = state.copy(gamePhase = GamePhase.DecisionsAccepted(
              gameId,
              totalPlayers.filterNot(p => p.id == playerId),
              playersBets.filterNot(_._1 == playerId),
              played.filterNot(p => p.id == playerId),
              folded.filterNot(p => p.id == playerId),
              playerHand
            )
            )
            (updatedState, playerLeaved)
          case GamePhase.Resolved(
              gameId,
              playerHand,
              dealerHand,
              outcome,
              totalPlayers,
          playersBets,
              played,
              folded
              ) =>
            val updatedState = state.copy(gamePhase = GamePhase.Resolved(
              gameId,
              playerHand,
              dealerHand,
              outcome,
              totalPlayers.filterNot(p => p.id == playerId),
              playersBets.filterNot(_._1 == playerId),
              played.filterNot(p => p.id == playerId),
              folded.filterNot(p => p.id == playerId)
            )
            )
            (updatedState, playerLeaved)
        }
      }
    } yield result

  def startNewGame(): IO[Either[WrongGamePhaseError, GameStarted]] =
    for {
      logger <- Slf4jLogger.fromName[IO]("start-game-logger")
      result <- gameState.modify { state =>
        state.gamePhase match {
          case GamePhase.WaitingForPlayers(players) =>
            val gameId = GameId(UUID.randomUUID)

            val updatedState =
              state.copy(gamePhase = GamePhase.Started(gameId, players))

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
      _ <- gameServerMessageService.gameStarted(result)
      _ <- logger.info(result.toString)
    } yield result

  def waitForBets(): IO[Either[WrongGamePhaseError, WaitForBet]] =
    for {
      logger <- Slf4jLogger.fromName[IO]("wait-bet-logger")
      result <- gameState.modify { state =>
        state.gamePhase match {
          case GamePhase.Started(gameId, players) =>
            val updatedState =
              state.copy(gamePhase = GamePhase.WaitingForBets(gameId, players, Map.empty))

            (updatedState, Right(WaitForBet()))
          case phase =>
            (
              state,
              Left(
                WrongGamePhaseError(
                  s"Cannot wait for bet when game phase is not started (current game phase: $phase)"
                )
              )
            )
        }
      }
      _ <- gameServerMessageService.waitingForBetsStarted(result)
      _ <- logger.info(result.toString)

    } yield result

  def acceptBet(player: Player, bet: Double): IO[Either[WrongGamePhaseError, BetAccepted]] =
    for {
      logger <- Slf4jLogger.fromName[IO]("accept-bet-logger")
      result <- gameState.modify { state =>
        state.gamePhase match {
          case GamePhase.WaitingForBets(gameId, allConnectedPlayers, playersWhoBet)
              if allConnectedPlayers.exists(_.id == player.id) =>
            val updatedState = state.copy(gamePhase = GamePhase
              .WaitingForBets(gameId, allConnectedPlayers, playersWhoBet.updated(player.id, bet))
            )
            (updatedState, Right(BetAccepted()))

          case phase =>
            (
              state,
              Left(
                WrongGamePhaseError(
                  s"Cannot accept bet when game phase is not waiting for bets (current game phase: $phase)"
                )
              )
            )
        }
      }
      _ <- gameServerMessageService.betAccepted(player, bet, result)
      _ <- logger.info(result.toString)
    } yield result

  def betsFinished(): IO[Either[WrongGamePhaseError, BetsFinished]] =
    for {
      logger     <- Slf4jLogger.fromName[IO]("bet-finished-logger")
      _          <- deck.resetAndShuffle
      playerHand <- deck.drawCards(isPlayerHand = true)
      result <- gameState.modify { state =>
        state.gamePhase match {
          case GamePhase.WaitingForBets(gameId, totalPlayers, playersWhoBet) =>
            val updatedState =
              state.copy(gamePhase =
                GamePhase.BetsAccepted(gameId, totalPlayers, playersWhoBet, playerHand)
              )
            (updatedState, Right(BetsFinished()))
          case phase =>
            (
              state,
              Left(
                WrongGamePhaseError(
                  s"Cannot finish bets game when game phase is not waiting for bets (current game phase: $phase)"
                )
              )
            )
        }
      }
      _ <- logger.info(result.toString)
      _ <- gameServerMessageService.waitingForDecisionsStarted(result, playerHand)
    } yield result

  def waitForDecisions(): IO[Either[WrongGamePhaseError, WaitForDecision]] =
    for {
      logger <- Slf4jLogger.fromName[IO]("accept-bet-logger")
      result <- gameState.modify { state =>
        state.gamePhase match {
          case GamePhase.BetsAccepted(gameId, totalPlayers, playersWhoBet, playerHand) =>
            val updatedState =
              state.copy(gamePhase = GamePhase
                .WaitingForDecisions(gameId, totalPlayers, playersWhoBet, Nil, Nil, playerHand)
              )
            (updatedState, Right(WaitForDecision()))
          case phase =>
            (
              state,
              Left(
                WrongGamePhaseError(
                  s"Cannot wait for decisions when game phase is not bets finished (current game phase: $phase)"
                )
              )
            )
        }
      }
      _ <- logger.info(result.toString)

    } yield result

  def acceptDecision(
    player: Player,
    decision: Decision
  ): IO[Either[WrongGamePhaseError, DecisionAccepted]] =
    for {
      logger <- Slf4jLogger.fromName[IO]("accept-decision-logger")
      result <- gameState.modify { state =>
        state.gamePhase match {
          case GamePhase.WaitingForDecisions(
              gameId,
              totalPlayers,
              playersWhoBet,
              willPlay,
              willFold,
              playerHand
              )
              if playersWhoBet.keys.contains(player.id) &&
                !willPlay.exists(_.id == player.id) &&
                !willFold.exists(_.id == player.id) =>
            val updatedState = decision match {
              case Decision.Play =>
                state.copy(gamePhase = GamePhase
                  .WaitingForDecisions(
                    gameId,
                    totalPlayers,
                    playersWhoBet,
                    willPlay :+ player,
                    willFold,
                    playerHand
                  )
                )
              case Decision.Fold =>
                state.copy(gamePhase = GamePhase
                  .WaitingForDecisions(
                    gameId,
                    totalPlayers,
                    playersWhoBet,
                    willPlay,
                    willFold :+ player,
                    playerHand
                  )
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
            totalPlayers,
            playersWhoBet,
            decidedToPlay,
            decidedToFold,
            playerHand
            ) =>
          val playersWhoBetIds = playersWhoBet.keySet
          val playersWithBet = totalPlayers.filter(player => playersWhoBetIds.contains(player.id))
          val autoFolded = playersWithBet.diff(decidedToPlay ++ decidedToFold)

          val totalFolded = decidedToFold ++ autoFolded
          val updatedState =
            state.copy(gamePhase = GamePhase.DecisionsAccepted(
              gameId,
              totalPlayers,
              playersWhoBet,
              decidedToPlay,
              totalFolded,
              playerHand
            )
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
          case GamePhase.DecisionsAccepted(
              gameId,
              totalPlayers,
              playersWhoBet,
              played,
              folded,
              playerHand
              ) =>
            val gameOutcome = HandComparisonUtil.compare(playerHand, dealerHand)
            val updatedState = state.copy(gamePhase = GamePhase.Resolved(
              gameId,
              playerHand,
              dealerHand,
              gameOutcome,
              totalPlayers,
              playersWhoBet,
              played,
              folded
            )
            )
            updatePlayerBalance(playersWhoBet, played, folded, gameOutcome)

            (
              updatedState,
              Right(
                GameResolved(dealerHand, totalPlayers, playersWhoBet, played, folded, gameOutcome)
              )
            )
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
        case GamePhase.Resolved(_, _, _, _, totalPlayers, _, _, _) =>
          val updatedState =
            state.copy(gamePhase = GamePhase.WaitingForPlayers(totalPlayers))
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

  private def updatePlayerBalance(
                                   playersWhoBet: Map[PlayerId, Double],
                                   played: List[Player],
                                   folded: List[Player],
                                   outcome: Outcome
                                 ): Unit = {
    playersWhoBet.foreach {
      case (playerId, betAmount) =>
        val player = findPlayerById(playerId, played ++ folded)
        player.foreach { p =>
          if (folded.contains(p)) {
            val updatedPlayer = p.updateBalance(-betAmount)
            replacePlayerInListsAndMap(p, updatedPlayer, played, folded, playersWhoBet)
          } else if (played.contains(p)) {
            outcome match {
              case Outcome.PlayerWon =>
                val updatedPlayer = p.updateBalance(2 * betAmount)
                replacePlayerInListsAndMap(p, updatedPlayer, played, folded, playersWhoBet)
              case Outcome.DealerWon =>
                val updatedPlayer = p.updateBalance(-2 * betAmount)
                replacePlayerInListsAndMap(p, updatedPlayer, played, folded, playersWhoBet)
              case _ => // No balance updates needed for tied outcome
            }
          }
        }
    }
  }

  private def findPlayerById(playerId: PlayerId, players: List[Player]): Option[Player] = {
    players.find(_.id == playerId)
  }

  private def replacePlayerInListsAndMap(
                                          oldPlayer: Player,
                                          newPlayer: Player,
                                          played: List[Player],
                                          folded: List[Player],
                                          playersWhoBet: Map[PlayerId, Double]
                                        ): Unit = {
    val updatedPlayersWhoBet = playersWhoBet.updated(oldPlayer.id, newPlayer.balance)

    val updatedPlayed = played.map {
      case p if p.id == oldPlayer.id => newPlayer
      case p => p
    }

    val updatedFolded = folded.map {
      case p if p.id == oldPlayer.id => newPlayer
      case p => p
    }

    played.clear()
    played.addAll(updatedPlayed)

    folded.clear()
    folded.addAll(updatedFolded)

    playersWhoBet.clear()
    playersWhoBet.addAll(updatedPlayersWhoBet)
  }


}

object GameProcessingService {
  final case class DecisionAccepted()
  final case class BetAccepted()
  final case class DecisionsFinished()
  final case class BetsFinished()
  final case class GameJoined()

  final case class PlayerLeaved(player: PlayerId)

  final case class GameResolved(
    dealerHand: Hand,
    totalPlayers: List[Player],
    playersWhoBet: Map[PlayerId, Double],
    played: List[Player],
    folded: List[Player],
    gameOutcome: Outcome
  )
  final case class GameStarted(gameId: GameId)
  final case class PlayersMoved()
  final case class WaitForDecision()
  final case class WaitForBet()

  final case class PlayerCount(value: BigDecimal) extends AnyVal

  def apply(
    gameState: Ref[IO, GameState],
    gameServerMessageService: GameServerMessageService,
    deck: Deck[IO]
  ) = new GameProcessingService(gameState, gameServerMessageService, deck)
}
