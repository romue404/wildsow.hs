module Main where

import Lib
import Model
import Actions
import Control.Monad

main :: IO ()
main = do
  let (hand, card) =  deck `deal` 5
  shuffled <- shuffle hand
  print shuffled
