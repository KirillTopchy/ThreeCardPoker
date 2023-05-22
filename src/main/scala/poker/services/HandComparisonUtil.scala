package poker.services

import poker.domain.game.Outcome
import poker.domain.player.{Hand, Pair}

object HandComparisonUtil {
  def compare(playerHand: Hand, dealerHand: Hand): Outcome =
    if (playerHand.combination.rank > dealerHand.combination.rank)
      Outcome.PlayerWon
    else if (playerHand.combination.rank < dealerHand.combination.rank)
      Outcome.DealerWon
    else
      compareHandsWithEqualRanks(playerHand, dealerHand)

  private def compareHandsWithEqualRanks(playerHand: Hand, dealerHand: Hand): Outcome =
    playerHand.combination match {
      case Pair => comparePairHands(playerHand, dealerHand)
      case _    => compareHandsByHighestCard(playerHand, dealerHand)
    }

  private def compareHandsByHighestCard(playerHand: Hand, dealerHand: Hand): Outcome = {
    val playerHighestCardValue = handHighestCardValue(playerHand)
    val dealerHighestCardValue = handHighestCardValue(dealerHand)

    if (playerHighestCardValue > dealerHighestCardValue)
      Outcome.PlayerWon
    else if (playerHighestCardValue < dealerHighestCardValue)
      Outcome.DealerWon
    else
      Outcome.Tied
  }

  private def comparePairHands(playerHand: Hand, dealerHand: Hand): Outcome = {
    val playerPairRank = pairHandPairCardsRankValue(playerHand)
    val dealerPairRank = pairHandPairCardsRankValue(dealerHand)

    if (playerPairRank > dealerPairRank)
      Outcome.PlayerWon
    else if (playerPairRank < dealerPairRank)
      Outcome.DealerWon
    else {
      val playerThirdCardRank = pairHandThirdCardRankValue(playerHand, playerPairRank)
      val dealerThirdCardRank = pairHandThirdCardRankValue(dealerHand, playerPairRank)

      if (playerThirdCardRank > dealerThirdCardRank)
        Outcome.PlayerWon
      else if (playerThirdCardRank < dealerThirdCardRank)
        Outcome.DealerWon
      else
        Outcome.Tied
    }
  }

  private def handHighestCardValue(hand: Hand): Int = hand.cards.map(_.rank.value).max

  private def pairHandPairCardsRankValue(hand: Hand): Int =
    hand.cards
      .groupBy(_.rank)
      .collect {
        case (rank, cards) if cards.length == 2 => rank.value
      }
      .max

  private def pairHandThirdCardRankValue(hand: Hand, pairRankValue: Int): Int =
    hand.cards.filterNot(_.rank.value == pairRankValue).head.rank.value
}
