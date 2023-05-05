package poker.services

import poker.domain.game.Outcome
import poker.domain.player.Hand

object HandComparisonUtil {

  // TODO: add hand evaluation logic.
  def compare(player: Hand, dealer: Hand): Outcome =
    if (player == dealer)
      Outcome.Tied
    else
      Outcome.PlayerWon
}