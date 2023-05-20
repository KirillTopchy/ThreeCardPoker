package poker.domain.card

final case class Card(rank: Rank, suit: Suit)

object Card {
  val KS = Card(Rank.King, Suit.Spades)
  val KH = Card(Rank.King, Suit.Hearts)
  val KD = Card(Rank.King, Suit.Diamonds)

  val AS = Card(Rank.Ace, Suit.Spades)
  val AH = Card(Rank.Ace, Suit.Hearts)
  val AD = Card(Rank.Ace, Suit.Diamonds)
}