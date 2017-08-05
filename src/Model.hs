module Model where

import Data.Map
import Types
import Requisites

-- import System.Random

data Card = Card {value :: Value, color :: Color} deriving (Read, Show, Eq)
data Color = Eichel | Gras | Herz | Schellen deriving (Read, Show, Enum, Eq, Bounded)
data Value =  Six| Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Read,  Show, Enum, Eq, Ord, Bounded)

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

data GamePhase = GameOver | WaitingForTricks Player | WaitingForColor Player | WaitingForCard Player  | Evaluation

data PlayerState = PlayerState {player :: Player, playedCard :: Maybe Card, hand :: Cards, tricks :: [Int], score :: [Integer], tricksSubround::[(Int,Int)]}

data GameState = GameState {
  phase :: GamePhase,
  currentRound :: Int,
  currentColor :: Maybe Color,
  pile :: Cards,
  trump :: Color,
  players :: [PlayerState]
}

