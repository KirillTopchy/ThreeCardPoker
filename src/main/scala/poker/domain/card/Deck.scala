package poker.domain.card

import cats.effect.{IO, Ref}
import poker.domain.player.Hand

import scala.util.Random

sealed trait Deck[F[_]] {
  def drawCards(isPlayerHand: Boolean): F[Hand]
  def cardsLeft: F[Int]
  def resetAndShuffle: F[Unit]
}

object Deck  {
  private val cardsDeck: List[Card] = for {
    rank <- Rank.allRanks
    suit <- Suit.allSuits
  } yield Card(rank, suit)

  private def shuffleDeck(): List[Card] = Random.shuffle(cardsDeck)

  def apply(): IO[Deck[IO]] = Ref.of[IO, List[Card]](shuffleDeck()).map { ref =>
    new Deck[IO] {
      override def drawCards(isPlayerHand: Boolean): IO[Hand] = for {
        cardDeck <- ref.get
        cards = if (isPlayerHand) cardDeck.take(3) else cardDeck.slice(3, 6)
      } yield Hand(cards, isPlayerHand)

      override def cardsLeft: IO[Int] = ref.get.map(_.length)

      override def resetAndShuffle: IO[Unit] = ref.update(_ => shuffleDeck()).void
    }
  }
}