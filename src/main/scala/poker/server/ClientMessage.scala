package poker.server

sealed trait ClientMessage

object ClientMessage {
  final case class Join()                extends ClientMessage
  final case class Bet(amount: Float)    extends ClientMessage
}