package poker.services

import poker.domain.card.{Card, Rank, Suit}
import poker.domain.player.{Hand, Player, PlayerId}

import java.util.UUID

trait GameProcessingServiceTestData {
  val firstPlayerId: PlayerId  = PlayerId(UUID.randomUUID())
  val secondPlayerId: PlayerId = PlayerId(UUID.randomUUID())
  val firstPlayer: Player      = Player(firstPlayerId)
  val secondPlayer: Player     = Player(secondPlayerId)
  val playerHand: Hand = Hand(
    List[Card](
      Card(Rank.King, Suit.Spades),
      Card(Rank.Queen, Suit.Diamonds),
      Card(Rank.Ace, Suit.Clubs)
    ), isPlayerHand = true
  )
  val dealerHand: Hand = Hand(
    List[Card](
      Card(Rank.King, Suit.Spades),
      Card(Rank.Queen, Suit.Diamonds),
      Card(Rank.Queen, Suit.Clubs)
    ), isPlayerHand = false
  )
}
