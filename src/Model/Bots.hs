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
botMove gs@GameState {players = playersStates, phase=phase}
    | (isWaitingForCards phase || isWaitingForTricks phase) =
        case currentPlayer of
            RandomBot _ -> Just $ randomBotMove gs currentPlayerState
            SmartBot _ -> Just $ smartBotMove gs currentPlayerState
            HumanPlayer _ -> Nothing
    | otherwise = Nothing
    where
        currentPlayerState = head playersStates
        currentPlayer = player currentPlayerState


-- RandomBot
randomBotMove :: GameState -> PlayerState -> PlayerMove
randomBotMove gs@GameState {phase = WaitingForTricks p} me = randomBotTricksToMake gs me
randomBotMove gs@GameState {phase = WaitingForCard p} me = randomBotCardToPlay gs me
randomBotMove gs me = randomBotCardToPlay gs me

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

-- SmartBot
-- smart tricks
-- play best cards until tricks reached, then play lowest cards
smartBotMove :: GameState -> PlayerState -> PlayerMove
smartBotMove gs@GameState {phase = WaitingForTricks p} me = smartBotTricksToMake gs me
smartBotMove gs@GameState {phase = WaitingForCard p} me = smartBotCardToPlay gs me

smartBotTricksToMake :: GameState -> PlayerState -> PlayerMove
smartBotTricksToMake gs@GameState{players = players, stdGen=gen} PlayerState{hand=hand, player=me} =
    let (rand, _) = randomR (0,length hand) gen
        amountOfPlayers = length players
    in TellNumberOfTricks me rand

smartBotCardToPlay :: GameState -> PlayerState -> PlayerMove
smartBotCardToPlay gs@GameState { trump=trump, currentColor=currentColor, stdGen=gen} PlayerState{hand=hand, player=me} =
    case currentColor of
        Nothing -> PlayCard me (randomCard hand gen)
        Just currentColor -> PlayCard me (randomCard (playeableCards2 trump currentColor hand) gen)

-- Helpers
cardWinningChance :: GameState -> Card -> Double
cardWinningChance gs@GameState{pile=pile, players=playersStates, currentColor=currentColor, trump=trump} card =
    let playersState = head playersStates
        possibleHigerCards = possibleHigherCards gs playersState card
    in  1.0 - fromIntegral (length possibleHigerCards) / fromIntegral (length deck)

-- only if the bot has to tell the color
possibleHigherCards :: GameState -> PlayerState -> Card -> Cards
possibleHigherCards gs@GameState{pile=pile, trump=trump} ps@PlayerState{hand=hand} card@Card{value=v} =
    let myUnknownCards = unknownCards gs ps
    in  filter (\Card{value=v'} -> v' > v) myUnknownCards -- map (\(value a -> a)) myUnknownCards

--
--possibleHigherCardsWithCurrentolor :: GameState -> Card -> Cards
--possibleHigherCardsWithCurrentolor gs@GameState{pile=pile, players=players, currentColor=currentColor, trump=trump} card =

-- cards that are possible in the opponents hands
unknownCards :: GameState -> PlayerState -> Cards
unknownCards gs@GameState{pile=pile} ps@PlayerState{hand=hand} = (deck \\ pile) \\ hand

-- playable cards
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
