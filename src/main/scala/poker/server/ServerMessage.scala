package poker.server

sealed trait ServerMessage

object ServerMessage {
  final case class Message(text: String) extends ServerMessage
}