package poker.domain.game

import poker.domain.player.{Hand, Player}

sealed trait GamePhase

object GamePhase {
  final case object WaitingForPlayers extends GamePhase
  final case object Test extends GamePhase

  final case class Started(gameId: GameId, playerHand: Hand, playing: List[Player]) extends GamePhase

  final case class WaitingForDecisions(gameId: GameId, playerHand: Hand, playing: List[Player]) extends GamePhase

  final case class DecisionsAccepted(gameId: GameId, playerHand: Hand, played: List[Player], folded: List[Player]) extends GamePhase

  final case class Resolved(gameId: GameId, playerHand: Hand, dealerHand: Hand, outcome: Outcome)
    extends GamePhase
}
