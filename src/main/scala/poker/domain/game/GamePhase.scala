package poker.domain.game

import poker.domain.player.{Hand, Player}

sealed trait GamePhase

object GamePhase {
  final case class WaitingForPlayers(joinedPlayers: List[Player]) extends GamePhase

  final case class Started(gameId: GameId, playing: List[Player])
      extends GamePhase

  final case class WaitingForBets(
    gameId: GameId,
    totalPlayers: List[Player],
    playersWhoBet: List[Player]
  ) extends GamePhase

  final case class BetsAccepted(
    gameId: GameId,
    totalPlayers: List[Player],
    playersWhoBet: List[Player],
    playerHand: Hand
  ) extends GamePhase

  final case class WaitingForDecisions(
    gameId: GameId,
    totalPlayers: List[Player],
    playersWhoBet: List[Player],
    willPlay: List[Player],
    willFold: List[Player],
    playerHand: Hand
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
