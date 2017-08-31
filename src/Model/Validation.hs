module Model.Validation where
import Model.Evaluation (cardsWithColor, cardsOnTable, orElseList)
import Data.Maybe (fromMaybe)
import Data.List (find)
import Model.Model
import Data.Maybe (isJust)



-- might use maybe alternative
playeableCards :: Player -> GameState -> Cards
playeableCards player gs@GameState{playerStates=players, trump=trump, currentColor=currentColor} =
  fromMaybe [] $ do -- TODO default this to hand
    currentColor' <- currentColor
    player' <- find (\p -> player ==  (Model.Model.player p)) players
    let table = cardsOnTable players
    let playersHand = hand player'
    let trumpsOnTable = cardsWithColor table trump
    let playersTrumps = cardsWithColor playersHand trump
    let playersCardsWithColor = cardsWithColor playersHand currentColor'
    case trumpsOnTable of
      (c:cs) -> Just $  playersCardsWithColor `orElseList` (playersTrumps `orElseList` playersHand)
      [] -> Just $  playersCardsWithColor `orElseList` playersHand


mustHoldOr :: Bool -> PlayerMoveError ->  Either PlayerMoveError ()
mustHoldOr True e  = Right ()
mustHoldOr False e = Left e

------------------------------------------- BOT INFO -------------------------------------------
countBots :: [PlayerState] -> [Int]
countBots ps = map (\PlayerState{player=p} -> case p of
                                                RandomBot _ -> 1
                                                otherwise -> 0) ps
amountOfBots :: [PlayerState] -> Int
amountOfBots ps = foldl (+) 0 (countBots ps)

------------------------------------------- PREDICATES -------------------------------------------

playerNameIsFree :: String -> [PlayerState] -> Bool
playerNameIsFree name ps = null $ filter (\PlayerState{player=p} -> (playerName p) == name) ps

everyPlayerPlayed :: GameState -> Bool
everyPlayerPlayed gameState = all (\p-> isJust $ playedCard p) $  playerStates gameState

allHandsPlayed :: GameState -> Bool
allHandsPlayed gs = all (\p -> null $ hand p) $ playerStates gs

allTricksSet :: GameState -> Bool
allTricksSet gameState =  flip(all) players' haveEnoughEntries
  where players' = playerStates gameState
        round = currentRound gameState
        haveEnoughEntries = (\PlayerState{tricks=t} -> length(t) >=  round)

enoughPlayers :: [PlayerState] -> Bool
enoughPlayers ps = length ps >= minAmountOfPlayers

mayJoin :: [PlayerState] -> Bool
mayJoin ps = length ps < maxAmountOfPlayers

isPlayersTurn :: Player -> GamePhase -> Bool
isPlayersTurn player (WaitingForCard player')   = player ==  player'
isPlayersTurn player (WaitingForTricks player') = player ==  player'
isPlayersTurn player (WaitingForColor player')  = player ==  player'

isWaitingForCards :: GamePhase -> Bool
isWaitingForCards (WaitingForCard _) = True
isWaitingForCards _ = False

isWaitingForTricks :: GamePhase -> Bool
isWaitingForTricks (WaitingForTricks  _) = True
isWaitingForTricks _ = False


isWaitingForColor :: GamePhase -> Bool
isWaitingForColor (WaitingForColor _) = True
isWaitingForColor _ = False

isIdle :: GamePhase -> Bool
isIdle Idle = True
isIdle _ = False

isBot :: Player -> Bool
isBot (HumanPlayer _) = False
isBot (RandomBot _) = True

isHuman :: Player -> Bool
isHuman = not . isBot

