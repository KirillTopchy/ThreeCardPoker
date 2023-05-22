package poker.domain.card

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import poker.domain.player.Hand

class DeckSpec extends AnyFreeSpec with Matchers {
  "Player and Dealer hands should contain 3 different cards" in {
    val deckIO: IO[Deck[IO]] = Deck()
    val deck: Deck[IO] = deckIO.unsafeRunSync()
    deck.resetAndShuffle

    val playerHand: Hand = deck.drawCards(isPlayerHand = true).unsafeRunSync()
    val dealerHand: Hand = deck.drawCards(isPlayerHand = false).unsafeRunSync()

    playerHand.cards should have size 3
    dealerHand.cards should have size 3

    val allCards = playerHand.cards ++ dealerHand.cards

    val distinctCards = allCards.distinct
    distinctCards should have size 6
  }
}
