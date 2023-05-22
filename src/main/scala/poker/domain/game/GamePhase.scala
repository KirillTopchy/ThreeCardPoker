package poker.domain.game

import poker.domain.player.{Hand, Player, PlayerId}

sealed trait GamePhase

object GamePhase {
  final case class WaitingForPlayers(joinedPlayers: List[Player]) extends GamePhase

  final case class Started(gameId: GameId, playing: List[Player]) extends GamePhase

  final case class WaitingForBets(
    gameId: GameId,
    totalPlayers: List[Player],
    playersBets: Map[PlayerId, Double],
  ) extends GamePhase

  final case class BetsAccepted(
    gameId: GameId,
    totalPlayers: List[Player],
    playersBets: Map[PlayerId, Double],
    playerHand: Hand
  ) extends GamePhase

  final case class WaitingForDecisions(
    gameId: GameId,
    totalPlayers: List[Player],
    playersBets: Map[PlayerId, Double],
    willPlay: List[Player],
    willFold: List[Player],
    playerHand: Hand
  ) extends GamePhase

  final case class DecisionsAccepted(
    gameId: GameId,
    totalPlayers: List[Player],
    playersBets: Map[PlayerId, Double],
    played: List[Player],
    folded: List[Player],
    playerHand: Hand
  ) extends GamePhase

  final case class Resolved(
    gameId: GameId,
    playerHand: Hand,
    dealerHand: Hand,
    outcome: Outcome,
    totalPlayers: List[Player],
    playersBets: Map[PlayerId, Double],
    played: List[Player],
    folded: List[Player]
  ) extends GamePhase
}
