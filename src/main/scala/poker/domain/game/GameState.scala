package poker.domain.game

import poker.domain.player.{Player, PlayerState}

final case class GameState private (gamePhase: GamePhase, players: Map[Player, PlayerState])

object GameState {
  val empty: GameState =
    GameState(gamePhase = GamePhase.WaitingForPlayers, Map.empty)
}
