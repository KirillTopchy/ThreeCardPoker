package poker

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import poker.domain.card.{Card, Rank, Suit}
import poker.domain.player.{Flush, Hand, HighCard, Pair, Straight, StraightFlush, ThreeOfAKind}

class HandSpec extends AnyFreeSpec with Matchers {
  val handKs2c5d: Hand = Hand(List(Card(Rank.King, Suit.Spades), Card(Rank.Two, Suit.Clubs), Card(Rank.Five, Suit.Diamonds)))
  val handKsKc5d: Hand = Hand(List(Card(Rank.King, Suit.Spades), Card(Rank.King, Suit.Clubs), Card(Rank.Five, Suit.Diamonds)))
  val handQh2h7h: Hand = Hand(List(Card(Rank.Queen, Suit.Hearts), Card(Rank.Two, Suit.Hearts), Card(Rank.Seven, Suit.Hearts)))
  val hand2h3d4h: Hand = Hand(List(Card(Rank.Two, Suit.Hearts), Card(Rank.Three, Suit.Diamonds), Card(Rank.Four, Suit.Hearts)))
  val hand7s7d7c: Hand = Hand(List(Card(Rank.Seven, Suit.Spades), Card(Rank.Seven, Suit.Diamonds), Card(Rank.Seven, Suit.Clubs)))
  val handJd9d10d: Hand = Hand(List(Card(Rank.Jack, Suit.Diamonds), Card(Rank.Nine, Suit.Diamonds), Card(Rank.Ten, Suit.Diamonds)))
  val handAh2h3h: Hand = Hand(List(Card(Rank.Ace, Suit.Hearts), Card(Rank.Two, Suit.Hearts), Card(Rank.Three, Suit.Hearts)))
  val handAhQhKh: Hand = Hand(List(Card(Rank.Ace, Suit.Hearts), Card(Rank.Queen, Suit.Hearts), Card(Rank.King, Suit.Hearts)))
  val handAh2h3d: Hand = Hand(List(Card(Rank.Ace, Suit.Hearts), Card(Rank.Two, Suit.Hearts), Card(Rank.Three, Suit.Diamonds)))

  "Hand score should be calculated correctly" - {
    handKs2c5d.score shouldBe 13 + 2 + 5
    handKsKc5d.score shouldBe 13 + 13 + 5
    handQh2h7h.score shouldBe 12 + 2 + 7
    hand2h3d4h.score shouldBe 2 + 3 + 4
    hand7s7d7c.score shouldBe 7 + 7 + 7
    handJd9d10d.score shouldBe 11 + 9 + 10
    handAh2h3h.score shouldBe 1 + 2 + 3
    handAhQhKh.score shouldBe 14 + 12 + 13
    handAh2h3d.score shouldBe 1 + 2 + 3
  }

  "Hand rank should be evaluated correctly" - {
    handKs2c5d.getRank shouldBe HighCard
    handKsKc5d.getRank shouldBe Pair
    handQh2h7h.getRank shouldBe Flush
    hand2h3d4h.getRank shouldBe Straight
    hand7s7d7c.getRank shouldBe ThreeOfAKind
    handJd9d10d.getRank shouldBe StraightFlush
    handAh2h3h.getRank shouldBe StraightFlush
    handAhQhKh.getRank shouldBe StraightFlush
    handAh2h3d.getRank shouldBe Straight
  }
}
