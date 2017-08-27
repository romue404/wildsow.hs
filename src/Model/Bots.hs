module Model.Bots where

import Model.Model as Model
import Model.Updates
import Data.List
import System.Random
import System.Random.Shuffle
import Model.Validation

-- am beste wir haben ein bots file aus dem man dann die verschiedenen bts aufrufen kann

aiMove :: PlayerState -> GameState -> PlayerMove
aiMove me gs@GameState {phase = WaitingForTricks p, players = players, stdGen=gen} = tricksToMake me (length players) gen
aiMove me gs@GameState {phase = (WaitingForCard p), players = players, trump=trump, currentColor=currentColor, stdGen=gen} = cardToPlay gs me gen

tricksToMake :: PlayerState -> int -> StdGen -> PlayerMove
tricksToMake PlayerState{hand=hand, player=me} amountOfPlayers gen = let (rand, _) = randomR (0,length hand) gen
                                                                     in TellNumberOfTricks (Model.playerName me) rand

cardToPlay :: GameState -> PlayerState  -> StdGen -> PlayerMove
cardToPlay gs PlayerState{hand=hand, player=me} gen = PlayCard (playerName me) (randomCard (playeableCards (playerName me) gs) gen)

randomCard :: Cards -> StdGen -> Card
randomCard cards gen = let (rand, _) = randomR (0,length cards) gen
                       in cards!!rand
