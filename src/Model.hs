module Model where

import Types
import Requisites
import System.Random(StdGen, newStdGen, next, mkStdGen)
import System.Random.Shuffle

data Card = Card {value :: Value, color :: Color} deriving (Read, Show, Eq)
data Color = Eichel | Gras | Herz | Schellen deriving (Read, Show, Enum, Eq, Bounded)
data Value =  Six| Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Read, Show, Enum, Eq, Ord, Bounded)

instance CardEq Card where
   colorEq (Card _ c) (Card _ c') =  c == c'
   valueEq (Card v _) (Card v' _) =  v == v'


-- implement ordering for cards
-- implement show for gameState

type Cards = [Card]
type PlayerNumber = Integer
data Player = Player {name :: String, number :: PlayerNumber} deriving (Read, Show, Eq)


colors = listAll::[Color]
values = listAll:: [Value]

deck :: Cards
deck = [Card v c | c <- colors, v <- values]

data PlayerMove = PlayCard Player Card | TellNumberOfTricks Player Int | TellColor Player Color deriving (Read, Show)

data GamePhase = Idle | GameOver | WaitingForTricks Player | WaitingForColor Player | WaitingForCard Player  | Evaluation deriving (Read, Show)

data PlayerState = PlayerState {player :: Player, playedCard :: Maybe Card, hand :: Cards, tricks :: [Int], score :: [Int], tricksSubround::[(Int,Int)]} deriving (Read, Show)

data GameState = GameState {
  phase :: GamePhase,
  currentRound :: Int,
  currentColor :: Maybe Color,
  pile :: Cards,
  trump :: Color,
  players :: [PlayerState],
  stdGen ::  StdGen
}


initWildsowGameState :: StdGen -> GameState
initWildsowGameState gen = GameState{
  phase = Idle,
  currentRound = 1,
  currentColor = Nothing,
  pile = shuffledPile',
  trump = color trump',
  players=(initPlayerState p1):(initPlayerState p2):[],
  stdGen=gen'
}
  where shuffledPile = shuffle' deck (length deck) gen
        gen' = snd $ next gen
        (trump':shuffledPile') = shuffledPile



initPlayerState player = PlayerState player Nothing [] [] [] []
p1 = Player "Thomas Mueller" 1
p2 = Player "James Roriguez" 2
