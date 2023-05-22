package poker.domain.player

sealed trait Decision

object Decision {
  final case object Play extends Decision
  final case object Fold extends Decision
}
