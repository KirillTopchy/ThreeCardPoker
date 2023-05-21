package poker.services

import cats.effect.IO
import org.typelevel.log4cats.SelfAwareStructuredLogger
import poker.domain.player.{Player, PlayerId}
import poker.server.ClientMessage
import poker.server.ClientMessage.{Join, PlayerDecision}

class ClientMessageProcessingService(
  gameProcessingService: GameProcessingService,
  logger: SelfAwareStructuredLogger[IO]
) {
  def processSingleMessage(playerId: PlayerId, clientMessage: ClientMessage): IO[Unit] =
    (clientMessage match {
      case Join() =>
        logger.info("Player joined") *> gameProcessingService.joinGame(Player(playerId))
      case PlayerDecision(decision) =>
        logger.info(s"Accepted decision $decision") *> gameProcessingService.acceptDecision(
          Player(playerId),
          decision
        )
    }).void
}

object ClientMessageProcessingService {
  def apply(gameProcessingService: GameProcessingService, logger: SelfAwareStructuredLogger[IO]) =
    new ClientMessageProcessingService(gameProcessingService, logger)
}
