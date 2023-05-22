package poker.domain.player

final case class Player (id: PlayerId, balance: Double = 500, bet: Double = 0){
  def updateBalance(amount: Double): Player = {
    copy(balance = balance + amount)
  }

  def placeBet(bet: Double): Player = {
    copy(bet = bet)
  }

  def clearBet(): Player = {
    copy(bet = 0)
  }
}
