package poker.server

sealed trait ServerMessage

final case class TestMessage(text: String) extends ServerMessage
