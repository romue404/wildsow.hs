module Model.Bots where

import Model.Model as Model
import Model.Updates
import Data.List
import System.Random
import System.Random.Shuffle
import Model.Validation
import System.Random.Shuffle


-- Spieltheorie
-- bei Poker um ein dynamisches Spiel mit unvollkommener Informationen und Zufallsereignissen. Poker ist dabei ein
-- strikt kompetatives Nullsummenspiel (einer gewinnt, alle anderen verlieren) und nicht symmetrisch,
-- da die zu w¨ahlenden Handlungsalternativen von der Position am Tisch abh¨angen.

--Pokerspiel um ein unvollkommenes dynamisches Nullsummenspiel mit Zufallseinfluß
-- Rückwärrtsinduktion


-- am beste wir haben ein bots file aus dem man dann die verschiedenen bts aufrufen kann

-- check if its a turn for a bot and do it
botMove :: GameState -> Maybe PlayerMove
botMove gs@GameState {players = playersStates} = let
  currentPlayerState = head playersStates
  currentPlayer = player currentPlayerState
  in case currentPlayer of
      RandomBot _ -> Just $ randomBotMove gs currentPlayerState
      HumanPlayer _ -> Nothing


-- RandomBot
randomBotMove :: GameState -> PlayerState -> PlayerMove
randomBotMove gs@GameState {phase = WaitingForTricks p} me = randomBotTricksToMake gs me
randomBotMove gs@GameState {phase = (WaitingForCard p)} me = randomBotCardToPlay gs me


randomBotTricksToMake :: GameState -> PlayerState -> PlayerMove
randomBotTricksToMake gs@GameState{players = players, stdGen=gen} PlayerState{hand=hand, player=me} =
    let (rand, _) = randomR (0,length hand) gen
        amountOfPlayers = length players
    in TellNumberOfTricks me rand


randomBotCardToPlay :: GameState -> PlayerState -> PlayerMove
randomBotCardToPlay gs@GameState { trump=trump, currentColor=currentColor, stdGen=gen} PlayerState{hand=hand, player=me} =
    case currentColor of
        Nothing -> PlayCard me (randomCard hand gen)
        Just currentColor -> PlayCard me (randomCard (playeableCards2 trump currentColor hand) gen)


-- Helpers
playeableCards2 :: Color -> Color -> Cards -> Cards
playeableCards2 trump currentColor hand
    | length cardsFitsCurrentColor > 0 = cardsFitsCurrentColor
    | length cardsFitsTrump > 0 = cardsFitsTrump
    | otherwise = hand
    where
        cardsFitsCurrentColor = cardsWithColor hand currentColor
        cardsFitsTrump = cardsWithColor hand trump

randomCard :: Cards -> StdGen -> Card
randomCard cards gen =
    let (rand, _) = randomR (0, (length cards)-1) gen
        shuffled = shuffle' cards (length cards) gen
    in head shuffled
