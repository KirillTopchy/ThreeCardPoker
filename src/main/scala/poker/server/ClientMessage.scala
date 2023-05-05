package poker.server

sealed trait ClientMessage

object ClientMessage {
  final case class Message(msg: String)  extends ClientMessage
}