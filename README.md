# 3-Card Poker Game in Scala

## Description

This Scala application is a web-based implementation of the popular card game "3-card poker". The game follows the standard rules of 3-card poker, where players are dealt three cards and can choose to fold or continue playing based on the strength of their hand.

## Installation

To install and run the game, follow these steps:

1. Ensure that you have Java and Scala installed on your machine.
2. Clone this repository main branch to your local machine using Git.
3. Navigate to the root directory of the cloned repository.
4. Run the command `sbt run` to start the web server.
5. Navigate to three-card-poker\src\main\scala\poker\server\WebServer.scala and run it.
6. Navigate to three-card-poker\frontend\index.html and open it with any browser.

## Features

The game includes the following features:

- A web-based user interface that allows players to interact with the game.
- The ability to join game.
- The ability to fold or continue playing based on the strength of the player's hand.
- The ability to view the outcome of the game and the winning hand.

## Technologies Used

The following technologies were used to develop this application:

- Scala
- HTTP4S
- ScalaTest
- Mockito
- Circe
- Cats Effects
- Cats Logging
- HTML/CSS/JavaScript

## Project Main Components
These components form the core functionality of the project, enabling the hosting and management of a poker game through a web server.

- **WebServer**: Represents a web server that hosts a poker game. It is responsible for establishing WebSocket connections and starting the Game Engine as a background task.

- **PokerRoutes**: Provides the implementation of HTTP routes for the poker server. It handles WebSocket connections, processes incoming client messages, and sends server messages to clients.

- **ClientMessageProcessingService**: Handles client messages/commands, such as player join requests and player decisions, and delegates the processing to the game processing service.

- **GameEngineService**: Responsible for managing the flow of the game. It repeatedly executes methods from the game processing service based on the current game state. This component waits for players to join, starts a new game, waits for player decisions, resolves the game, and repeats the process for subsequent games.

- **GameProcessingService**: Handles the logic related to game phases, player actions, and game state transitions. The service maintains the current game state using a mutable Ref from the Cats Effect library. It interacts with the game server message service and the deck to perform various operations.

- **GameServerMessageService**: Handles game-related messages and sends them to players. It provides methods for various game events such as game start, player join, decision acceptance, and game resolution.

- **HandComparisonUtil**: Provides utility methods for comparing hands in a poker game. This component is used to determine the outcome of a player's hand compared to the dealer's hand.

## Project Visual Representation

![20230523_191201](https://github.com/KirillTopchy/ThreeCardPoker/assets/89339107/f0f126c2-8f55-4025-8820-7e60230009aa)

