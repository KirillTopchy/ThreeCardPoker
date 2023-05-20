package poker.domain.game

import poker.domain.player.Player

final case class Game (id: GameId, players: List[Player])
