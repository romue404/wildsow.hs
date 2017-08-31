module Model.Bots where

import Model.Model as Model
import Model.Updates
import Data.List
import System.Random
import System.Random.Shuffle
import Model.Validation
import System.Random.Shuffle
import Data.Function (on)


-- Spieltheorie
-- bei Poker um ein dynamisches Spiel mit unvollkommener Informationen und Zufallsereignissen. Poker ist dabei ein
-- strikt kompetatives Nullsummenspiel (einer gewinnt, alle anderen verlieren) und nicht symmetrisch,
-- da die zu w¨ahlenden Handlungsalternativen von der Position am Tisch abh¨angen.

--Pokerspiel um ein unvollkommenes dynamisches Nullsummenspiel mit Zufallseinfluß
-- Rückwärrtsinduktion


-- am beste wir haben ein bots file aus dem man dann die verschiedenen bts aufrufen kann

-- check if its a turn for a bot and do it
botMove :: GameState -> Maybe PlayerMove
botMove gs@GameState {playerStates = playersStates, phase=phase}
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
randomBotTricksToMake gs@GameState{playerStates = players, stdGen=gen} PlayerState{hand=hand, player=me} =
    let (rand, _) = randomR (0,length hand) gen
        amountOfPlayers = length players
    in TellNumberOfTricks me rand

randomBotCardToPlay :: GameState -> PlayerState -> PlayerMove
randomBotCardToPlay gs@GameState { trump=trump, currentColor=currentColor, stdGen=gen} PlayerState{hand=hand, player=me} =
    case currentColor of
        Nothing -> PlayCard me (randomCard hand gen)
        Just currentColor -> PlayCard me (randomCard (playeableCards2 trump currentColor hand) gen)

{-
SmartBot
smart tricks
play highest cards until tricks reached, then play lowest cards
play highest card if it is possible to win else lowest card
-}
smartBotMove :: GameState -> PlayerState -> PlayerMove
smartBotMove gs@GameState {phase = WaitingForTricks p} me = smartBotTricksToMake gs me
smartBotMove gs@GameState {phase = WaitingForCard p} me = smartBotCardToPlay gs me

--
smartBotTricksToMake :: GameState -> PlayerState -> PlayerMove
smartBotTricksToMake gs@GameState{playerStates = players} PlayerState{hand=hand, player=me} =
    let chances = map (\card -> cardWinningChance gs card) hand
        chancesAvg =  (sum chances) / (genericLength chances)
        predictedTricks = round chancesAvg * genericLength hand
    in TellNumberOfTricks me predictedTricks

--
smartBotCardToPlay :: GameState -> PlayerState -> PlayerMove
smartBotCardToPlay gs@GameState { trump=trump, currentColor=currentColor, stdGen=gen} ps@PlayerState{hand=hand, player=me} =
    case currentColor of
        -- set the color
        Nothing -> if belowTricks gs ps
            then PlayCard me $ fst $ head $ sortedHighestHandCards gs ps -- play highest card
            else PlayCard me $ fst $ last $ sortedHighestHandCards gs ps-- play lowest card
        -- play a card
        Just currentColor -> PlayCard me (randomCard (playeableCards2 trump currentColor hand) gen)

------- Helpers
belowTricks :: GameState -> PlayerState -> Bool
belowTricks GameState{currentRound=currentRound} ps@PlayerState{tricks=tricks, tricksSubround=tricksSubround} =
    let toldTricksThisRound = head tricks
        tricksInThisRound = foldl (\a (_,s) -> a+s) 0 (filter (\(r,_) -> currentRound == r) tricksSubround)
    in toldTricksThisRound < tricksInThisRound

sortedHighestHandCards:: GameState -> PlayerState -> [(Card, Double)]
sortedHighestHandCards gs PlayerState{hand=hand} =
    let zipped = zip (hand) (map (cardWinningChance gs) hand)
    in sortBy (flip compare `on` snd) zipped

cardWinningChance :: GameState -> Card -> Double
cardWinningChance gs@GameState{pile=pile, playerStates=playersStates, currentColor=currentColor, trump=trump} card =
    let playersState = head playersStates
        possibleHigerCards = possibleHigherCards gs playersState card
    in  1.0 - (genericLength possibleHigerCards) / genericLength (unknownCards gs playersState) - genericLength pile -- check unknownCards or deck

-- only if the bot has to tell the color
possibleHigherCards :: GameState -> PlayerState -> Card -> Cards
possibleHigherCards gs@GameState{pile=pile, trump=trump} ps@PlayerState{hand=hand} card@Card{value=v} =
    let myUnknownCards = unknownCards gs ps
    in  filter (\Card{value=v'} -> v' > v) myUnknownCards -- map (\(value a -> a)) myUnknownCards

-- with current color
possibleHigherCardsWithCurrentColor :: GameState -> PlayerState -> Card -> Cards
possibleHigherCardsWithCurrentColor gs@GameState{currentColor=currentColor, trump=trump} ps@PlayerState{hand=hand} card@Card{value=v, color=c}
    -- higher cards: higher trumps
    | currentColorJust == c && trump==c = filter (\Card{value=v', color=c'} -> v'>v && c'==trump) myUnknownCards
    -- higher cards: trumps and higher currentColor
    | currentColorJust == c             = filter (\Card{value=v', color=c'} -> c'==trump || c'==currentColorJust && v'>v) myUnknownCards
    -- higher cards: all others
    | currentColorJust /= c             = myUnknownCards -- and trump add possible trump cards?!??!?
    | otherwise                         = myUnknownCards -- todo
    where
        myUnknownCards    = unknownCards gs ps
        wc                = filter (\Card{value=v', color=c'} -> v' > v || c' == currentColorJust) myUnknownCards
        currentColorJust  = case currentColor of
            Nothing -> trump
            Just currentColor -> currentColor

-- cards that are possible in the opponents hands
unknownCards :: GameState -> PlayerState -> Cards
unknownCards gs@GameState{pile=pile} ps@PlayerState{hand=hand} = ((deck \\ pile) \\ hand ) \\ opponentPlayedCards gs
-- unknownCards gs@GameState{pile=pile} ps@PlayerState{hand=hand} = map (\\) [deck, pile, hand, opponentPlayedCards gs]

-- cards the opponent played before my turn
opponentPlayedCards :: GameState -> Cards
opponentPlayedCards gs@GameState{playerStates=playerStates} =
      cardsOnTable playerStates -- todo check
--    let playedPlayerStates = filter (\PlayerState{playedCard=Just playedCard} -> isJust playedCard ) playerStates
--        aaa = fromJust playedCard playerStates
--    in map (playedCard) $ filter (\card@Card{color=c', value=v'} -> playedCard card) playedPlayerStates

-- playable cards
playeableCards2 :: Color -> Color -> Cards -> Cards
playeableCards2 trump currentColor hand
    | length cardsFitsCurrentColor > 0 = cardsFitsCurrentColor
    | length cardsFitsTrump > 0 = cardsFitsTrump
    | otherwise = hand
    where
        cardsFitsCurrentColor = filter (\Card{value=v', color=c'} ->  c' == currentColorJust) hand
        cardsFitsTrump        = filter (\Card{value=v', color=c'} ->  c' == trump) hand
        currentColorJust      = case currentColor of
            isNothing -> trump
            isJust -> currentColor

randomCard :: Cards -> StdGen -> Card
randomCard cards gen =
    let (rand, _) = randomR (0, (length cards)-1) gen
        shuffled = shuffle' cards (length cards) gen
    in head shuffled
