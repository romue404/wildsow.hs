module ModelUpdates where

import Model
import Data.List
import Data.Ord
import Data.Maybe
import Control.Monad
import Types
import System.Random.Shuffle
import System.Random
import Control.Applicative

----------------------------------- STEPPING THE GAME FORWARD -----------------------------------
----------------------------------- PATTERN: VALIDATION -> PROCESS -> STEP ----------------------
moveValidataion :: PlayerMove -> Model.GameState -> Either Model.PlayerMoveError Model.GameState
moveValidation move@(PlayCard name card) gs@GameState{phase=p,  players=players} =
  checkMove [(enoughPlayers players, NotEnoughPlayers),
             (isWaitingForCards p, UnexpectedMove),
             (isPlayersTurn name p, NotPlayersTurn),
             (card `elem` playeableCards name gs, MoveAgainstRules "You are not allowed to play this card")
             ]
             move gs
moveValidataion move@(TellNumberOfTricks name tricks) gs@GameState{phase=p, players=players} =
  checkMove [(enoughPlayers players, NotEnoughPlayers),
            (isWaitingForTricks p, UnexpectedMove),
            (isPlayersTurn name p, NotPlayersTurn), (tricks >= 0, MoveAgainstRules "Tricks must be >= 0")
            ]
            move gs

moveValidataion move@(TellColor name color) gs@GameState{phase=p, players=players} =
  checkMove [(enoughPlayers players, NotEnoughPlayers),
            (isWaitingForColor p, UnexpectedMove),
            (isPlayersTurn name p, NotPlayersTurn)] move gs

moveValidataion move@(Join p) gs@GameState{phase=phase, players=players} =
  checkMove [(isIdle phase || mayJoin players, GameFull),
    (playerNameIsFree (playerName p) players, NameTaken)] move gs

moveValidataion move@(Leave p) gs= loginLogic p gs -- pass through

processMove :: PlayerMove -> GameState-> GameState
processMove (PlayCard name card) gs = gs{players = cardPlayedUpdate card (HumanPlayer name) $ Model.players gs}
processMove (TellNumberOfTricks name tricks) gs =  gs{players = tricksPlayerUpdate tricks (HumanPlayer name) $ Model.players gs}
processMove (TellColor _ color) gs = gs{currentColor=Just color}
processMove (Join player) gs = addPlayers [player] gs
processMove (Leave p) gs@Model.GameState{players=ps} = gs{players= (replaceHumanPlayerWithBot p Model.ai1 ps)} --TODO add random bot

step :: GameState -> GameState
step gs@GameState {phase = Idle} = (waitForColor . setNewTrump . dealCards) gs
step gs@GameState {phase = WaitingForTricks p}
  |allTricksSet gs = waitForNextCard gs
  |otherwise = waitForNextTricks gs
step gs@GameState {phase = WaitingForCard p}
  |everyPlayerPlayed gs = step gs{phase=Evaluation}
  |otherwise = waitForNextCard gs
step gs@GameState {phase=WaitingForColor p} =
  case currentColor gs of
    Nothing -> gs
    Just c -> gs{phase = WaitingForTricks p}
step gs@GameState {phase = Evaluation, currentRound=round}
  |not $ allHandsPlayed gs = (clearPlayedCards . setNewTrump . waitForNextCard . evaluateSubRound) gs
  |round+1 >= length  (cardsPerRound Model.deck $ length $ players gs) = (evaluateRound. evaluateSubRound) gs{phase=GameOver}
  |otherwise = (waitForColor . clearPlayedCards . setNewTrump . dealCards . evaluateRound. evaluateSubRound) gs{players = nextPlayer $ players gs}
  --  new round means we have to change the player twice
step gs@GameState {phase = GameOver} = gs

checkMove :: [(Bool, Model.PlayerMoveError)] -> Model.PlayerMove -> Model.GameState -> Either Model.PlayerMoveError Model.GameState
checkMove preds move  gs = gameStateOrError
  where unpassed = filter (\(a,b) -> a == False) preds
        gameStateOrError = case unpassed of
          [] -> Right $ (step . processMove(move)) gs
          ((_, e):es) -> Left e

----------------------------------- HELPER FUNCTIONS -----------------------------------

evaluateRound :: GameState -> GameState
evaluateRound gameState = gameState{players = playersWithScore, currentRound = round + 1}
  where round = currentRound gameState
        players' = players gameState
        playersWithScore = map (\p@PlayerState{tricksSubround=tricksList, tricks=toldTricks, score=currentScore} ->
          let tricksInThisRound = foldl (\a (_,s) -> a+s) 0 (filter (\(r,_) -> round == r) tricksList)
              toldTricksThisRound = head toldTricks
              score = if tricksInThisRound == toldTricksThisRound then (if tricksInThisRound == 0 then 5 else tricksInThisRound + 10) else tricksInThisRound
          in p{score = [score] ++ currentScore}) players'


evaluateSubRound :: GameState -> GameState
evaluateSubRound gameState =
  let round = Model.currentRound gameState
      trump = Model.trump $ gameState
      color = Model.currentColor gameState
      players = Model.players gameState
      candidatesTrump = [(p, c) | PlayerState{player=p, playedCard=Just c} <- players,  Model.color c ==  trump]
      candidatesColor = [(p, c) | PlayerState{player=p, playedCard=Just c} <- players, fromMaybe False $ (==) <$> Just (Model.color c) <*>  color]
      -- alle player mit einem gespielten trumpf
      -- alle spieler mit einer gespielten karte die der angesagten farbe entspricht
      -- wenn keinen gespielten trumpf gibt dann evaluiere die karten mit der angesagten farbe, die hoechste gewinnt
      -- wenn mindestens ein tumpf gespielt wurdde dann evaluiere diese, der hoehere gewinnt
      winner = if (not . null) candidatesTrump then highestCard candidatesTrump else highestCard candidatesColor
  in gameState{players = updatePlayer (\p -> p{tricksSubround = [(round, 1)] ++ tricksSubround p}) winner players}


cardsOnTable :: [PlayerState] -> Cards
cardsOnTable ps = catMaybes $ playedCard `fmap` ps

orElseList :: [a] -> [a] -> [a]
orElseList (x:xs) fallback = (x:xs)
orElseList [] fallback = fallback

-- could be done with Maybe type as well
playeableCards :: String -> GameState -> Cards
playeableCards player gs@GameState{players=players, trump=trump, currentColor=currentColor} =
  fromMaybe [] $ do
    currentColor' <- currentColor
    player' <- find (\p -> player ==  Model.playerName (Model.player p)) players
    let table = cardsOnTable players
    let playersHand = hand player'
    let trumpsOnTable = cardsWithColor table trump
    let playersTrumps = cardsWithColor playersHand trump
    let playersCardsWithColor = cardsWithColor playersHand currentColor'
    case trumpsOnTable of
      (c:cs) -> Just $  playersCardsWithColor `orElseList` (playersTrumps `orElseList` playersHand)
      [] -> Just $  playersCardsWithColor `orElseList` playersHand

-- show this in presentation
dealCards :: GameState -> GameState
dealCards gs@GameState{currentRound=round, pile=pile, players=players, stdGen = gen} =
  gs{pile=undealtCards, players=playersWithDealtCards, stdGen=gen'}
    where numberOfCards = (cardsPerRound Model.deck $ length players) !! round
          (shuffledDeck, gen') = (shuffle' Model.deck (length Model.deck) gen, snd $ next gen)
          chunked = sublist numberOfCards shuffledDeck
          playersWithDealtCards = map (\(chunk, player) ->player{hand=chunk, playedCard=Nothing}) (zip chunked players)
          undealtCards = concat $ drop (length players) chunked

cardsWithColor :: Cards -> Color -> Cards
cardsWithColor hand color = filter (\(Card v c) ->  c == color) hand

cardsPerRound :: Cards -> Int -> [Int]
cardsPerRound deck players =
  map (\x -> abs(x - rounds) + 1) [1..rounds * 2 - 1]
  where rounds = maxCardsPerPlayer deck players

maxCardsPerPlayer :: Cards -> Int -> Int
maxCardsPerPlayer deck numberOfPlayers =  (length deck) `div` numberOfPlayers

sublist :: Int -> [a] -> [[a]]
sublist n ls
    | n <= 0 || null ls = []
    | otherwise = take n ls:sublist n (drop n ls)

highestCard :: [(Player, Card)] -> Player
highestCard pcs = fst$ maximumBy (comparing(value . snd)) pcs

-- TODO avoid duplication
waitForColor :: GameState -> GameState
waitForColor gameState =  gameState{players = playerQueue, phase = WaitingForColor nextInLine, currentColor = Nothing}
  where playerQueue = nextPlayer(players gameState)
        nextInLine = (player . head) playerQueue

-- TODO avoid duplication
waitForNextTricks :: GameState -> GameState
waitForNextTricks gameState =  gameState{players = playerQueue, phase = WaitingForTricks nextInLine}
  where playerQueue = nextPlayer(players gameState)
        nextInLine = (player . head) playerQueue
-- TODO avoid duplication
waitForNextCard :: GameState -> GameState
waitForNextCard gameState =  gameState{players = playerQueue, phase = WaitingForCard nextInLine}
  where playerQueue = nextPlayer(players gameState)
        nextInLine = (player . head) playerQueue

clearPlayedCards :: GameState -> GameState
clearPlayedCards gameState =
  let players = Model.players gameState
      players' = map (\ps -> ps{playedCard=Nothing}) players
  in gameState {players = players'}

setNewTrump :: GameState -> GameState
setNewTrump gameState = gameState {trump= trump, pile = rest}
  where (trump, rest) = case pile gameState of
                        [] -> ((shuffle' Model.colors (length Model.colors) $ Model.stdGen gameState) !! 0, [])
                        (x:xs) -> (Model.color x, xs)

tricksPlayerUpdate :: Int -> Player -> [PlayerState] -> [PlayerState]
tricksPlayerUpdate tricks = updatePlayer $ tellTricks tricks

tellTricks :: Int -> PlayerState -> PlayerState
tellTricks tricks playerState = playerState{tricks = [tricks] ++ Model.tricks playerState }

cardPlayedUpdate :: Card -> Player -> [PlayerState] -> [PlayerState]
cardPlayedUpdate card = updatePlayer $ playCard card

playCard :: Card -> PlayerState -> PlayerState
playCard card playerState = playerState{playedCard=Just(card), hand= delete card (Model.hand playerState)}

updatePlayer :: (PlayerState->PlayerState) -> Player -> [PlayerState] -> [PlayerState]
updatePlayer f p ps = map (\x -> if player x == p then f(x) else x) ps

addPlayers :: [Player] -> GameState -> GameState
addPlayers newPlayers gs@GameState{players=players} = gs{players= players ++ (map initPlayerState newPlayers) }

countBots :: [PlayerState] -> [Int]
countBots ps = map (\PlayerState{player=p} -> case p of
                                                Ai _ -> 1
                                                otherwise -> 0) ps

amountOfBots :: [PlayerState] -> Int
amountOfBots ps = foldl (+) 0 (countBots ps)
------------------------------------------- PREDICATES -------------------------------------------



playerNameIsFree :: String -> [PlayerState] -> Bool
playerNameIsFree name ps = null $ filter (\PlayerState{player=p} -> (Model.playerName p) == name) ps

everyPlayerPlayed :: GameState -> Bool
everyPlayerPlayed gameState = all (\p-> isNothing $ playedCard p) $  players gameState

allHandsPlayed :: GameState -> Bool
allHandsPlayed gs = all (\p -> null $ hand p) $ players gs

isRoundStarter :: Player -> GameState -> Bool
isRoundStarter p gameState =  p == (player $ head $ players gameState) &&  (all (\p -> isNothing $ playedCard p) $ players gameState)

nextPlayer :: [PlayerState] -> [PlayerState]
nextPlayer (p:ps) = ps ++ [p]

allTricksSet :: GameState -> Bool
allTricksSet gameState =  flip(all) players' haveEnoughEntries
  where players' = players gameState
        round = currentRound gameState
        haveEnoughEntries = (\PlayerState{tricks=t} -> length(t) >=  round)


initPlayerState player = PlayerState player Nothing [] [] [] []

enoughPlayers :: [PlayerState] -> Bool
enoughPlayers ps = length ps >= Model.minAmountOfPlayers

mayJoin :: [PlayerState] -> Bool
mayJoin ps = length ps < Model.maxAmountOfPlayers

isPlayersTurn :: String -> GamePhase -> Bool
isPlayersTurn player (WaitingForCard player')   = player == playerName player'
isPlayersTurn player (WaitingForTricks player') = player == playerName player'
isPlayersTurn player (WaitingForColor player')  = player == playerName player'

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
isBot (Ai _) = True

isHuman :: Player -> Bool
isHuman = not . isBot


----------------- LOGIN ------------------------- TODO rename waiting for state

replacePlayer :: Player -> PlayerState -> PlayerState
replacePlayer newPlayer ps@PlayerState{player=p} = ps{player=newPlayer}

replaceBotWithPlayer :: Player -> [PlayerState] -> [PlayerState]
replaceBotWithPlayer newPlayer [] = []
replaceBotWithPlayer newPlayer (p@PlayerState{player=plr}:ps)
  |isHuman plr = p: (replaceBotWithPlayer newPlayer ps)
  |isBot plr = p{player=newPlayer}: ps

replaceHumanPlayerWithBot :: Player -> Player -> [PlayerState] -> [PlayerState]
replaceHumanPlayerWithBot player bot ps = updatePlayer (\p -> p{player=bot}) player ps

loginLogic :: Player -> GameState ->  Either Model.PlayerMoveError Model.GameState
loginLogic p gs@GameState{phase=Idle, players=ps}
  |length ps < maxAmountOfPlayers = Right (processMove (Join p) gs)
  |amountOfBots ps > 0 = Right (gs{players= (p `replaceBotWithPlayer` ps)})
  |otherwise = Left GameFull
loginLogic p gs@GameState{players=ps}
  |amountOfBots ps > 0 = Right (gs{players= (p `replaceBotWithPlayer` ps)})
  |otherwise = Left GameFull


--playerReplacement :: Player -> Player -> GameState -> GameState
--playerReplacement oldPlayer newPlayer gameState 