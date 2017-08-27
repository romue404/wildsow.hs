module Model.Bots where

import Model.Model as Model
import Model.Updates
import Data.List
import System.Random
import System.Random.Shuffle
import Model.Validation

-- am beste wir haben ein bots file aus dem man dann die verschiedenen bts aufrufen kann

-- check if its a turn for a bot and do it
botMove :: GameState -> Maybe PlayerMove
botMove gs@GameState {players = playersStates} = let
  currentPlayerState = head playersStates
  currentPlayer = player currentPlayerState
  in case currentPlayer of
      RandomBot _ -> Just $ randomBotMove currentPlayerState gs
      HumanPlayer _ -> Nothing


-- RandomBot
randomBotMove :: PlayerState -> GameState -> PlayerMove
randomBotMove me gs@GameState {phase = WaitingForTricks p, players = players, stdGen=gen} = randomBotTricksToMake me (length players) gen
randomBotMove me gs@GameState {phase = (WaitingForCard p), players = players, trump=trump, currentColor=currentColor, stdGen=gen} = randomBotCardToPlay gs me gen

randomBotTricksToMake :: PlayerState -> int -> StdGen -> PlayerMove
randomBotTricksToMake PlayerState{hand=hand, player=me} amountOfPlayers gen = let (rand, _) = randomR (0,length hand) gen
                                                                     in TellNumberOfTricks me rand

randomBotCardToPlay :: GameState -> PlayerState  -> StdGen -> PlayerMove
randomBotCardToPlay gs PlayerState{hand=hand, player=me} gen = PlayCard me (randomCard (playeableCards me gs) gen)




-- Helpers
randomCard :: Cards -> StdGen -> Card
randomCard cards gen = let (rand, _) = randomR (0,length cards) gen
                       in cards!!rand
