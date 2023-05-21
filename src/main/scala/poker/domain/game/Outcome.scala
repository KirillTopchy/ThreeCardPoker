package poker.domain.game

sealed trait Outcome

object Outcome {
  final case object PlayerWon extends Outcome

  final case object DealerWon extends Outcome

  final case object Tied extends Outcome
//
//  final case object Folded extends Outcome
}
