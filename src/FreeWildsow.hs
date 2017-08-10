{-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts #-}

module FreeWildsow where

import Control.Monad.Free
import Control.Monad.Free.TH
import System.Random
import Requisites

data GameOperations state message next =
    InitState next
  | AwaitStateCondition (state -> Bool) (state -> next)
  | ModifyState (state -> state) next
  | GetState (state -> next)
  | ReceiveMessage (message -> next)
  | End
  deriving (Functor) -- needs DeriveFunctor
makeFree ''GameOperations -- needs Flexible Contexts + template haskell

data Card = Card {value :: Value, color :: Color} deriving (Read, Show, Eq)
data Color = Eichel | Gras | Herz | Schellen deriving (Read, Show, Enum, Eq, Bounded)
data Value =  Six| Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Read, Show, Enum, Eq, Ord, Bounded)

type Cards = [Card]
type PlayerNumber = Integer
data Player = Player {name :: String, number :: PlayerNumber} deriving (Read, Show, Eq)


colors = listAll::[Color]
values = listAll:: [Value]

deck :: Cards
deck = [Card v c | c <- colors, v <- values]

data WildsowMessages = PlayCard Player Card | TellNumberOfTricks Player Int | TellColor Player Color deriving (Read, Show)

data GamePhase = Idle | GameOver | WaitingForTricks Player | WaitingForColor Player | WaitingForCard Player  | Evaluation deriving (Read, Show)

data PlayerState = PlayerState {player :: Player, playedCard :: Maybe Card, hand :: Cards, tricks :: [Int], score :: [Int], tricksSubround::[(Int,Int)]} deriving (Read, Show)

data WildsowState = WildsowState {
  phase :: GamePhase,
  currentRound :: Int,
  currentColor :: Maybe Color,
  pile :: Cards,
  trump :: Color,
  players :: [PlayerState],
  stdGen ::  StdGen
}

type WildsowOperations = GameOperations WildsowState WildsowMessages
type Wildsow = Free WildsowOperations