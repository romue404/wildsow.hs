module Main where

import Lib
import Model
import Actions
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import ModelUpdates
import System.Random

type App = StateT GameState IO ()

initGameStateM = return () :: State GameState ()

wildsow :: App
wildsow = do
  modify(step . processMove(TellColor p1 Herz))
  modify(step . processMove(TellNumberOfTricks p1 3))
  modify(step . processMove(TellNumberOfTricks p2 3))
  modify(step . processMove(PlayCard p1 Card{value=Ten, color=Herz}))
  s <- get
  lift $ print $ length $ players s
  lift $ print $ players s
  lift $ print $ allTricksSet s
  lift $ print $ phase s
  lift $ print $ trump s
  return()


main = do
  gen <- getStdGen
  evalStateT wildsow $ initWildsowGameState gen


