package poker.domain.game

import poker.domain.player.{Hand, Player}

sealed trait GamePhase

object GamePhase {
  final case class WaitingForPlayers(joinedPlayers: List[Player]) extends GamePhase

  final case class Started(gameId: GameId, playerHand: Hand, playing: List[Player])
      extends GamePhase

  final case class WaitingForDecisions(
    gameId: GameId,
    playerHand: Hand,
    totalPlayers: List[Player],
    willPlay: List[Player],
    willFold: List[Player]
  ) extends GamePhase

  final case class DecisionsAccepted(
    gameId: GameId,
    playerHand: Hand,
    played: List[Player],
    folded: List[Player]
  ) extends GamePhase

  final case class Resolved(
    gameId: GameId,
    playerHand: Hand,
    dealerHand: Hand,
    outcome: Outcome,
    playerWithGameOutcome: List[Player],
    folded: List[Player]
  ) extends GamePhase
}
