package poker.server

import io.circe.{Codec, Decoder, Encoder, KeyDecoder, KeyEncoder}
import io.circe.generic.semiauto.deriveCodec
import poker.domain.card.Card
import poker.domain.game.{GameId, GamePhase, GameState, Outcome}
import poker.domain.player.{Decision, Hand, Player, PlayerId, PlayerState}
import poker.domain.card.{Rank, Suit}

import java.util.UUID

object JsonCodec {
  implicit val decisionCodec: Codec[Decision]           = deriveCodec[Decision]
  implicit val clientMessageCodec: Codec[ClientMessage] = deriveCodec[ClientMessage]
  implicit val serverMessageCodec: Codec[ServerMessage] = deriveCodec[ServerMessage]

  implicit val rankDecoder: Decoder[Rank] =
    Decoder.forProduct2("value", "symbol")((_: String, symbol: String) => Rank.getRank(symbol))
  implicit val rankEncoder: Encoder[Rank] =
    Encoder.forProduct2("value", "symbol")(r => (r.value, r.symbol))

  implicit val suitDecoder: Decoder[Suit]           = Decoder.forProduct1("name")(Suit.getSuit)
  implicit val suitEncoder: Encoder[Suit]           = Encoder.forProduct1("name")(_.name)
  implicit val cardCodec: Codec[Card]               = deriveCodec[Card]
  implicit val handCodec: Codec[Hand]               = deriveCodec[Hand]
  implicit val gameIdCodec: Codec[GameId]           = deriveCodec[GameId]
  implicit val playerIdCodec: Codec[PlayerId]       = deriveCodec[PlayerId]
  implicit val playerCodec: Codec[Player]           = deriveCodec[Player]
  implicit val outcomeCodec: Codec[Outcome]         = deriveCodec[Outcome]
  implicit val gamePhaseCodec: Codec[GamePhase]     = deriveCodec[GamePhase]
  implicit val playerStateCodec: Codec[PlayerState] = deriveCodec[PlayerState]
  implicit val playerKeyEncoder: KeyEncoder[Player] = (p: Player) => p.id.toString
  implicit val playerKeyDecoder: KeyDecoder[Player] = (key: String) =>
    Some(Player(PlayerId(UUID.fromString(key))))
  implicit val gameStateCodec: Codec[GameState] = deriveCodec[GameState]
}
