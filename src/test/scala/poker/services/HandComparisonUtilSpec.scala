package poker.services

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import poker.domain.card.{Card, Rank, Suit}
import poker.domain.player.Hand
import poker.domain.game.Outcome

class HandComparisonUtilSpec extends AnyFreeSpec with Matchers {

  "Should be expected outcomes for Non Pair combinations" in {
    val firstPlayerHand: Hand = Hand(
      List(
        Card(Rank.King, Suit.Spades),
        Card(Rank.Two, Suit.Clubs),
        Card(Rank.Five, Suit.Diamonds)
      ),
      isPlayerHand = true
    )

    val secondPlayerHand: Hand = Hand(
      List(
        Card(Rank.Jack, Suit.Spades),
        Card(Rank.Two, Suit.Clubs),
        Card(Rank.Five, Suit.Diamonds)
      ),
      isPlayerHand = true
    )

    val firstDealerHand: Hand = Hand(
      List(
        Card(Rank.Queen, Suit.Spades),
        Card(Rank.Two, Suit.Clubs),
        Card(Rank.Four, Suit.Diamonds)
      ),
      isPlayerHand = true
    )

    val secondDealerHand: Hand = Hand(
      List(
        Card(Rank.Jack, Suit.Spades),
        Card(Rank.Two, Suit.Clubs),
        Card(Rank.Four, Suit.Diamonds)
      ),
      isPlayerHand = true
    )

    val playerWonOutcome = HandComparisonUtil.compare(firstPlayerHand, firstDealerHand)
    val dealerWonOutcome = HandComparisonUtil.compare(secondPlayerHand, firstDealerHand)
    val tiedOutcome      = HandComparisonUtil.compare(secondPlayerHand, secondDealerHand)

    playerWonOutcome shouldBe Outcome.PlayerWon
    dealerWonOutcome shouldBe Outcome.DealerWon
    tiedOutcome shouldBe Outcome.Tied
  }

  "Should be expected outcomes for Pair combinations" in {
    val firstPlayerHand: Hand = Hand(
      List(
        Card(Rank.King, Suit.Spades),
        Card(Rank.King, Suit.Clubs),
        Card(Rank.Five, Suit.Diamonds)
      ),
      isPlayerHand = true
    )

    val secondPlayerHand: Hand = Hand(
      List(
        Card(Rank.Jack, Suit.Spades),
        Card(Rank.Jack, Suit.Clubs),
        Card(Rank.Five, Suit.Diamonds)
      ),
      isPlayerHand = true
    )

    val thirdPlayerHand: Hand = Hand(
      List(
        Card(Rank.Jack, Suit.Spades),
        Card(Rank.Jack, Suit.Clubs),
        Card(Rank.Four, Suit.Diamonds)
      ),
      isPlayerHand = true
    )

    val fourthPlayerHand: Hand = Hand(
      List(
        Card(Rank.Jack, Suit.Spades),
        Card(Rank.Jack, Suit.Clubs),
        Card(Rank.Three, Suit.Diamonds)
      ),
      isPlayerHand = true
    )

    val firstDealerHand: Hand = Hand(
      List(
        Card(Rank.Queen, Suit.Spades),
        Card(Rank.Queen, Suit.Clubs),
        Card(Rank.Four, Suit.Diamonds)
      ),
      isPlayerHand = true
    )

    val secondDealerHand: Hand = Hand(
      List(
        Card(Rank.Jack, Suit.Spades),
        Card(Rank.Jack, Suit.Clubs),
        Card(Rank.Four, Suit.Diamonds)
      ),
      isPlayerHand = true
    )

    val playerWonOutcome     = HandComparisonUtil.compare(firstPlayerHand, firstDealerHand)
    val dealerWonOutcome     = HandComparisonUtil.compare(secondPlayerHand, firstDealerHand)
    val playerWonByThirdCard = HandComparisonUtil.compare(secondPlayerHand, secondDealerHand)
    val dealerWonByThirdCard = HandComparisonUtil.compare(fourthPlayerHand, secondDealerHand)
    val tiedOutcome          = HandComparisonUtil.compare(thirdPlayerHand, secondDealerHand)

    playerWonOutcome shouldBe Outcome.PlayerWon
    dealerWonOutcome shouldBe Outcome.DealerWon
    playerWonByThirdCard shouldBe Outcome.PlayerWon
    dealerWonByThirdCard shouldBe Outcome.DealerWon
    tiedOutcome shouldBe Outcome.Tied
  }
}
