package poker.domain.card

import cats.effect.{IO, Ref}
import scala.util.Random

sealed trait Deck[F[_]] {
  def drawCards(cardsCount: Int): F[List[Card]]
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
      override def drawCards(count: Int): IO[List[Card]] = for {
        cards <- ref.get
        _ <- ref.update(_.drop(count))
      } yield cards.take(count)

      override def cardsLeft: IO[Int] = ref.get.map(_.length)

      override def resetAndShuffle: IO[Unit] = ref.update(_ => shuffleDeck()).void
    }
  }
}