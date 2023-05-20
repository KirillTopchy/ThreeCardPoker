package poker.domain.game

import poker.domain.player.Player

final case class GameState (gamePhase: GamePhase)

object GameState {
  val empty: GameState =
    GameState(gamePhase = GamePhase.WaitingForPlayers(List.empty[Player]))
}