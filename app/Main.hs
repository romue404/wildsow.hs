module Main where

import Lib
import Model
import Actions
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import ModelUpdates

type App = StateT GameState IO

initPlayerState player = PlayerState player Nothing [] [] [] []
p1 = Player "Thomas Mueller" 1
p2 = Player "James Roriguez" 2
initGameState = GameState {
  phase = WaitingForColor p1,
  currentRound = 0,
  currentColor = Just Schellen,
  pile = deck,
  trump = Schellen,
  players = (initPlayerState p1):(initPlayerState p2):[]
}


initGameStateM = return () :: State GameState ()

testM = do
  s <- get
  lift (print $ currentColor s)
  modify(processMove(TellColor p1 Herz))
  s' <- get
  lift (print $ currentColor s')
  return ()


mainS = evalStateT testM initGameState

main :: IO ()
main = do
  let (hand, card) =  deck `deal` 5
  let (trump, tail) = drawTrump deck
  shuffled <- shuffle hand
  print shuffled
  print $ maxCardsPerPlayer deck 4
  print $ trump
  print $ cardsPerRound deck 4
  print $ nextPlayerNumber 1 4
  print $ playeableCards hand Gras Gras
  let (a,s) = runState initGameStateM $ setNewTrump initGameState
  print(Model.trump s)
  return ()
