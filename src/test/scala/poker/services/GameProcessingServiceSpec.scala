package poker.services

import cats.effect.{IO, Ref}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar.mock
import poker.domain.card.Deck
import poker.domain.game.GamePhase.{WaitingForDecisions, WaitingForPlayers}
import poker.domain.game.GameState
import poker.domain.player.Decision
import poker.services.GameProcessingService.{
  DecisionAccepted,
  DecisionsFinished,
  GameJoined,
  GameResolved,
  GameStarted,
  PlayersMoved
}

class GameProcessingServiceSpec
    extends AnyFreeSpec
    with Matchers
    with GameProcessingServiceTestData {

  "Game state change simulation test" in {
    val gameServerMessageService: GameServerMessageService = mock[GameServerMessageService]
    val deck: Deck[IO]                                     = mock[Deck[IO]]
    for {
      ref <- Ref.of[IO, GameState](GameState.empty)
      gameProcessingService = GameProcessingService(ref, gameServerMessageService, deck)
      firstPlayerJoined   <- gameProcessingService.joinGame(firstPlayer)
      secondPlayerJoined  <- gameProcessingService.joinGame(secondPlayer)
      gameStarted         <- gameProcessingService.startNewGame()
      waitingForDecisions <- gameProcessingService.waitForDecisions()
      firstPlayerDecisionAccepted <- gameProcessingService.acceptDecision(
        firstPlayer,
        Decision.Play
      )
      secondPlayerDecisionAccepted <- gameProcessingService.acceptDecision(
        secondPlayer,
        Decision.Fold
      )
      decisionProcessingFinished <- gameProcessingService.decisionsFinished()

      gameResolved   <- gameProcessingService.resolveGame()
      playersMoved   <- gameProcessingService.waitNextGamePlayers()
      finalGameState <- ref.get
    } yield {
      firstPlayerJoined shouldBe GameJoined: Unit
      secondPlayerJoined shouldBe GameJoined: Unit
      gameStarted shouldBe GameStarted: Unit
      waitingForDecisions shouldBe WaitingForDecisions: Unit
      firstPlayerDecisionAccepted shouldBe DecisionAccepted: Unit
      secondPlayerDecisionAccepted shouldBe DecisionAccepted: Unit
      decisionProcessingFinished shouldBe DecisionsFinished: Unit
      gameResolved shouldBe GameResolved: Unit
      playersMoved shouldBe PlayersMoved: Unit

      finalGameState.gamePhase shouldBe WaitingForPlayers: Unit
    }
  }
}
