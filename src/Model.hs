module Model where

import Data.Map
-- import System.Random

data Card = Card {value :: Value, color :: Color} deriving (Read, Show, Eq)
data Color = Eichel | Gras | Herz | Schellen deriving (Read, Show, Enum, Eq, Ord)
data Value = Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Read,  Show, Enum, Eq, Ord)

type Cards = [Card]
type PlayerNumber = Integer
data Player = Player {name :: String, number :: PlayerNumber}


colors :: [Color]
colors = [Eichel .. Schellen]

values :: [Value]
values = [Six .. Ace]

deck :: Cards
deck = [Card v c | c <- colors, v <- values]

data PlayerMove = PlayCard Card | TellNumberOfTricks Integer deriving (Read, Show, Eq)
data GameAction = WaitingForTricks PlayerNumber

data PlayerState = PlayerState {hand :: Cards, tricks :: [Integer], score :: [Integer]}
data GameState = GameState {
  pile :: Cards,
  trump :: Card,
  playersTurn :: Player,
  playersState :: Map Player PlayerState }

{-|
  cardsPerPlayer round:Int  draw a specified ammount of cards from the deck
  draw amount: Int  draw a specified ammount of cards from the deck
  draw trump
  required -> gameState -> Color | Value tells you wich kind of card you need to play
  isValidMove -> Boolean tells you if your move was correct
  tellNumberOfTricks -> Player tell the game how many tricks you would like to make, save tricks to game state
  makeMove
  evaluateRound
  setUpNextRound
  isGameOver
  makeGameSummary
  Geber -> cannot estimate #ofTricks so that they match the number of rounds
  allPossibleCards
-}
