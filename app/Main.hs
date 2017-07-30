module Main where

import Lib
import Model
import Actions
import Control.Monad

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
  print $ allowedToPlay hand Gras Schellen
