module Main where

import Model.Model
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Model.Updates
import System.Random
import GameNetwork.GameSocket

type App = StateT GameState IO ()

initGameStateM = return () :: State GameState ()

wildsow :: App
wildsow = do
  s <- get
  lift $ print $ length $ players s
  lift $ print $ length . hand $  players s !! 0
  lift $ print $ allTricksSet s
  lift $ print $ phase s
  lift $ print $ trump s
  lift $ print $ pile s
  return()

main :: IO()
main = do
  print("wildsow")

