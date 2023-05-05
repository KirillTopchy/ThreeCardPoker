package poker.server

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

object JsonCodec {
  implicit val clientMessageCodec: Codec[ClientMessage] = deriveCodec[ClientMessage]
  implicit val serverMessageCodec: Codec[ServerMessage] = deriveCodec[ServerMessage]
}
