package poker.domain.card

sealed trait Suit {
  val name: String
}

case object Suit {
  val allSuits: List[Suit] = List(Clubs, Diamonds, Hearts, Spades)

  def getSuit(suitName: String): Suit = suitName match {
    case "clubs"   => Clubs
    case "diamond" => Diamonds
    case "hearts"  => Hearts
    case "spades"  => Spades
  }

  case object Clubs extends Suit {
    override val name: String = "clubs"
  }

  case object Diamonds extends Suit {
    override val name: String = "diamond"
  }

  case object Hearts extends Suit {
    override val name: String = "hearts"
  }

  case object Spades extends Suit {
    override val name: String = "spades"
  }
}