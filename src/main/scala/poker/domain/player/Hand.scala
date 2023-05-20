package poker.domain.player

import poker.domain.card.Card

sealed trait HandRank {
  val handValue: Int
}

case object HighCard extends HandRank {
  override val handValue: Int = 1
}
case object Pair extends HandRank {
  override val handValue: Int = 2
}
case object Flush extends HandRank {
  override val handValue: Int = 3
}
case object Straight extends HandRank {
  override val handValue: Int = 4
}
case object ThreeOfAKind extends HandRank {
  override val handValue: Int = 5
}
case object StraightFlush extends HandRank {
  override val handValue: Int = 6
}

final case class Hand (cards: List[Card], isPlayerHand: Boolean){
  val score: Int = getHandScore

  private def getHandScore: Int = {
    if (isLowAceStraight) 6
    else this.cards.map(_.rank.value).sum
  }

  private def sort: List[Card] =
    cards.sortBy(_.rank.value).reverse

  private def isLowAceStraight = {
    val sortedCards = this.sort
    sortedCards.head.rank.value == 14 && sortedCards(1).rank.value == 3 && sortedCards.last.rank.value == 2
  }

  def getRank: HandRank = {
    val sortedCards = this.sort
    val isPair = this.cards.map(_.rank).distinct.length == 2
    val isFlush = this.cards.map(_.suit).distinct.length == 1
    val isStraight = {
      val ranks = sortedCards.map(_.rank.value)
      val minRank = ranks.min
      val maxRank = ranks.max
      val midRank = ranks.sum - minRank - maxRank
      val isNormalStraight = (maxRank - midRank == 1) && (midRank - minRank == 1)
      isLowAceStraight || isNormalStraight
    }
    val isThreeOfAKind = this.cards.map(_.rank).distinct.length == 1

    (isFlush, isStraight, isThreeOfAKind, isPair) match {
      case (true, true, _, _) => StraightFlush
      case (_, _, true, _) => ThreeOfAKind
      case (_, true, _, _) => Straight
      case (true, _, _, _) => Flush
      case (_, _, _, true) => Pair
      case _ => HighCard
    }
  }
}
