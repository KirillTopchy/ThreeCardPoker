package poker.server

sealed trait ServerMessage

object ServerMessage {
  final case class Message(msg: String) extends ServerMessage
}
