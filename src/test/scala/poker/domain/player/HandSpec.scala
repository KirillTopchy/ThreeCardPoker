package poker.domain.player

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import poker.domain.card.{Card, Rank, Suit}

class HandSpec extends AnyFreeSpec with Matchers {
  val handKs2c5d: Hand = Hand(
    List(Card(Rank.King, Suit.Spades), Card(Rank.Two, Suit.Clubs), Card(Rank.Five, Suit.Diamonds)),
    isPlayerHand = true
  )
  val handKsKc5d: Hand = Hand(
    List(Card(Rank.King, Suit.Spades), Card(Rank.King, Suit.Clubs), Card(Rank.Five, Suit.Diamonds)),
    isPlayerHand = true
  )
  val handQh2h7h: Hand = Hand(
    List(Card(Rank.Queen, Suit.Hearts), Card(Rank.Two, Suit.Hearts), Card(Rank.Seven, Suit.Hearts)),
    isPlayerHand = true
  )
  val hand2h3d4h: Hand = Hand(
    List(
      Card(Rank.Two, Suit.Hearts),
      Card(Rank.Three, Suit.Diamonds),
      Card(Rank.Four, Suit.Hearts)
    ),
    isPlayerHand = true
  )
  val hand7s7d7c: Hand = Hand(
    List(
      Card(Rank.Seven, Suit.Spades),
      Card(Rank.Seven, Suit.Diamonds),
      Card(Rank.Seven, Suit.Clubs)
    ),
    isPlayerHand = true
  )
  val handJd9d10d: Hand = Hand(
    List(
      Card(Rank.Jack, Suit.Diamonds),
      Card(Rank.Nine, Suit.Diamonds),
      Card(Rank.Ten, Suit.Diamonds)
    ),
    isPlayerHand = true
  )
  val handAh2h3h: Hand = Hand(
    List(Card(Rank.Ace, Suit.Hearts), Card(Rank.Two, Suit.Hearts), Card(Rank.Three, Suit.Hearts)),
    isPlayerHand = true
  )
  val handAhQhKh: Hand = Hand(
    List(Card(Rank.Ace, Suit.Hearts), Card(Rank.Queen, Suit.Hearts), Card(Rank.King, Suit.Hearts)),
    isPlayerHand = true
  )
  val handAh2h3d: Hand = Hand(
    List(Card(Rank.Ace, Suit.Hearts), Card(Rank.Two, Suit.Hearts), Card(Rank.Three, Suit.Diamonds)),
    isPlayerHand = true
  )

  "Hand rank should be evaluated correctly" - {
    handKs2c5d.combination shouldBe HighCard
    handKsKc5d.combination shouldBe Pair
    handQh2h7h.combination shouldBe Flush
    hand2h3d4h.combination shouldBe Straight
    hand7s7d7c.combination shouldBe ThreeOfAKind
    handJd9d10d.combination shouldBe StraightFlush
    handAh2h3h.combination shouldBe StraightFlush
    handAhQhKh.combination shouldBe StraightFlush
    handAh2h3d.combination shouldBe Straight
  }
}
