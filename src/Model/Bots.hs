module Bots where

import Model.Model as Model
import Model.Updates
import Data.List
import System.Random
import System.Random.Shuffle

-- am beste wir haben ein bots file aus dem man dann die verschiedenen bts aufrufen kann

aiMove :: PlayerState -> GameState -> PlayerMove
aiMove me gs@GameState {phase = WaitingForTricks p, players = players, stdGen=gen} = tricksToMake me (length players) gen
--aiMove me gs@GameState {phase = WaitingForCard p}
--  |everyPlayerPlayed gs = step gs{phase=Evaluation}
--  |otherwise = waitForNextCard gs


tricksToMake :: PlayerState -> int -> StdGen -> PlayerMove
tricksToMake PlayerState{hand=hand, player=me} amountOfPlayers gen = let (rand, _) = randomR (0,length hand) gen
                                                                     in TellNumberOfTricks (Model.playerName me) rand