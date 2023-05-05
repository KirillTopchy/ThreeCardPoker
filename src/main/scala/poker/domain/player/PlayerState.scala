package poker.domain.player

import poker.domain.game.Outcome

sealed trait PlayerState

object PlayerState {
  final case object Playing extends PlayerState

  final case class DecisionMade(decision: Decision) extends PlayerState

  final case class OutcomeGiven(outcome: Outcome, decision: Decision) extends PlayerState
}
