{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveAnyClass , FlexibleContexts #-}

module Model where

import Types
import Requisites
import System.Random(StdGen, newStdGen, next, mkStdGen)
import System.Random.Shuffle
import Data.Aeson.TH(deriveJSON, defaultOptions)
import Data.Aeson as Aeson(encode, decode, eitherDecode)

data Card = Card {value :: Value, color :: Color} deriving (Read, Show, Eq)
data Color = Eichel | Gras | Herz | Schellen deriving (Read, Show, Enum, Eq, Bounded)
data Value =  Six| Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Read, Show, Enum, Eq, Ord, Bounded)

type Cards = [Card]
data Player = Player {name :: String} deriving (Read, Show, Eq)

colors = listAll::[Color]
values = listAll:: [Value]

minAmountOfPlayers :: Int
minAmountOfPlayers = 3

maxAmountOfPlayers :: Int
maxAmountOfPlayers = 6

deck :: Cards
deck = [Card v c | c <- colors, v <- values]

data PlayerMove = PlayCard String Card
  | TellNumberOfTricks String Int
  | TellColor String Color deriving (Read, Show, Eq)

data GamePhase = Idle | GameOver | WaitingForTricks Player | WaitingForColor Player | WaitingForCard Player  | Evaluation deriving (Read)

data PlayerState = PlayerState {player :: Player, playedCard :: Maybe Card, hand :: Cards, tricks :: [Int], score :: [Int], tricksSubround::[(Int,Int)]} deriving (Read, Show)

data PlayerMoveError = NotPlayersTurn | MoveAgainstRules | UnexpectedMove deriving Show

data GameState = GameState {
  phase :: GamePhase,
  currentRound :: Int,
  currentColor :: Maybe Color,
  pile :: Cards,
  trump :: Color,
  players :: [PlayerState],
  stdGen ::  StdGen
}

instance CardEq Card where
   colorEq (Card _ c) (Card _ c') =  c == c'
   valueEq (Card v _) (Card v' _) =  v == v'

instance PlayerAction PlayerMove where
  whos (TellNumberOfTricks name _) = name
  whos (PlayCard name _) = name
  whos (TellColor name _) = name

instance Show GamePhase where
  show (Idle) = "Idle"
  show  (GameOver) = "Game Over"
  show (WaitingForTricks player) = "Waiting for player " `mappend` show (name player) `mappend` " to tell his tricks"
  show (WaitingForColor player) = "Waiting for player " `mappend` show (name player) `mappend` " to tell the color"
  show (WaitingForCard player) = "Waiting for player " `mappend` show (name player) `mappend` " to play a card"
  show (Evaluation) = "Evaluation"

deriveJSON defaultOptions ''PlayerState
deriveJSON defaultOptions ''GamePhase
deriveJSON defaultOptions ''Player
deriveJSON defaultOptions ''Value
deriveJSON defaultOptions ''Color
deriveJSON defaultOptions ''Card


