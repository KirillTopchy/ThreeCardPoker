package poker.server

import poker.domain.player.Decision

sealed trait ClientMessage

object ClientMessage {
  final case class Join()                             extends ClientMessage
  final case class PlayerDecision(decision: Decision) extends ClientMessage
  final case class Bet(amount: Float)                 extends ClientMessage
}
