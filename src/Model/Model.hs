{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveAnyClass , FlexibleContexts, RecordWildCards #-}

module Model.Model where

import Model.Types
import System.Random(StdGen, newStdGen, next, mkStdGen)
import System.Random.Shuffle
import Data.Aeson.TH(deriveJSON, defaultOptions)
import Data.Aeson as Aeson hiding (Value)
import Data.Ord(comparing)

data Card = Card {value :: Value, color :: Color} deriving (Read, Show, Eq)
data Color = Eichel | Gras | Herz | Schellen deriving (Read, Show, Enum, Eq, Bounded)
data Value =   Seven | Eight | Nine | Jack | Queen | King | Ten | Ace deriving (Read, Show, Enum, Eq, Ord, Bounded)
type Cards = [Card]
data Player = HumanPlayer {playerName :: String} | RandomBot {playerName :: String} | SmartBot {playerName :: String} deriving (Read, Show, Eq, Ord)


listAll :: (Enum a, Bounded a) => [a]
listAll = [minBound .. maxBound]

colors = listAll::[Color]
values = listAll:: [Value]

minAmountOfPlayers :: Int
minAmountOfPlayers = 3

maxAmountOfPlayers :: Int
maxAmountOfPlayers = 6

deck :: Cards
deck = [Card v c | c <- colors, v <- values]

data PlayerMove =
    PlayCard Player Card
  | TellNumberOfTricks Player Int
  | Join Player
  | Leave Player
  | Begin deriving (Read, Show, Eq)

data GamePhase = Idle | GameOver | WaitingForTricks Player | WaitingForColor Player | WaitingForCard Player  | Evaluation  deriving (Read, Eq)

data PlayerState = PlayerState {player :: Player, playedCard :: Maybe Card, hand :: Cards, tricks :: [Int], score :: [Int], tricksSubround::[(Int,Int)]} deriving (Read, Show, Eq)

data PlayerMoveError = NotPlayersTurn | MoveAgainstRules {reason::String} | UnexpectedMove {reason::String} | NotEnoughPlayers | GameFull | NameTaken  deriving (Show)

data GameState = GameState {
  phase :: GamePhase,
  currentRound :: Int,
  currentColor :: Maybe Color,
  pile :: Cards,
  discardPile :: [(String, Int, Card)],
  trump :: Color,
  playerStates :: [PlayerState],
  stdGen ::  StdGen
}

instance CardEq Card where
   colorEq (Card _ c) (Card _ c') =  c == c'
   valueEq (Card v _) (Card v' _) =  v == v'

instance PlayerAction PlayerMove where
  whos (TellNumberOfTricks player _ ) = playerName player
  whos (PlayCard player _) = playerName player
  whos (Join p) = playerName p
  whos (Leave p) = playerName p

instance Show GamePhase where
  show (Idle) = "Idle"
  show  (GameOver) = "Game Over"
  show (WaitingForTricks player) = "Waiting for player " `mappend` show (playerName player) `mappend` " to tell his tricks"
  show (WaitingForColor player) = "Waiting for player " `mappend` show (playerName player) `mappend` " to tell the color"
  show (WaitingForCard player) = "Waiting for player " `mappend` show (playerName player) `mappend` " to play a card"
  show (Evaluation) = "Evaluation"

deriveJSON defaultOptions ''PlayerState
deriveJSON defaultOptions ''PlayerMoveError
deriveJSON defaultOptions ''GamePhase
deriveJSON defaultOptions ''Player
deriveJSON defaultOptions ''Value
deriveJSON defaultOptions ''Color
deriveJSON defaultOptions ''Card


instance ToJSON GameState where
  toJSON GameState{..} = object [
    "phase" .= show phase,
    "round"  .= currentRound,
    "color" .= currentColor,
    "trump"  .= trump,
    "playerState" .= playerStates,
    "playedCards" .= discardPile
    ]


-- INIT --
initWildsowGameState :: StdGen -> GameState
initWildsowGameState gen =  GameState{
  phase = Idle,
  currentRound = 1,
  currentColor = Nothing,
  pile = [],
  trump = Gras,
  playerStates= [],
  discardPile = [],
  stdGen=gen
}

bot1 = RandomBot "Thomas Mueller"
bot2 = RandomBot "James Roriguez"
bot3 = RandomBot "Arjen Robben"
bot4 = RandomBot "Frank Ribery"
