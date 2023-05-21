package poker.server

import io.circe.{Codec, Decoder, Encoder, Json, KeyDecoder, KeyEncoder}
import io.circe.generic.semiauto.{deriveCodec, deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import poker.domain.card.Card
import poker.domain.game.{GameId, GamePhase, GameState, Outcome}
import poker.domain.player.{Combination, Decision, Hand, Player, PlayerId}
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
  implicit val combinationCodec: Codec[Combination] = deriveCodec[Combination]

  implicit val handCodec: Codec[Hand] = {
    val encoder: Encoder[Hand] = (hand: Hand) => {
      val baseJson = deriveEncoder[Hand].apply(hand).dropNullValues
      baseJson.deepMerge(Json.obj("combination" -> hand.combination.asJson))
    }
    val decoder: Decoder[Hand] = deriveDecoder[Hand]
    Codec.from(decoder, encoder)
  }

  implicit val gameIdCodec: Codec[GameId]           = deriveCodec[GameId]
  implicit val playerIdCodec: Codec[PlayerId]       = deriveCodec[PlayerId]
  implicit val playerCodec: Codec[Player]           = deriveCodec[Player]
  implicit val outcomeCodec: Codec[Outcome]         = deriveCodec[Outcome]
  implicit val gamePhaseCodec: Codec[GamePhase]     = deriveCodec[GamePhase]
  implicit val playerKeyEncoder: KeyEncoder[Player] = (p: Player) => p.id.toString
  implicit val playerKeyDecoder: KeyDecoder[Player] = (key: String) =>
    Some(Player(PlayerId(UUID.fromString(key))))
  implicit val gameStateCodec: Codec[GameState] = deriveCodec[GameState]
}
