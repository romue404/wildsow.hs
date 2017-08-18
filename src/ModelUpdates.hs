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


moveValidataion :: PlayerMove -> Model.GameState -> Either Model.PlayerMoveError Model.GameState
moveValidation move@(PlayCard name card) gs@GameState{phase=p} =
  checkMove [(isWaitingForCards p, UnexpectedMove),
             (isPlayersTurn (Player name) gs, NotPlayersTurn),
             (card `elem` playeableCards (Player name) gs, MoveAgainstRules "You are not allowed to play this card")]
             move gs
moveValidataion move@(TellNumberOfTricks name tricks) gs@GameState{phase=p} =
  checkMove [(isWaitingForTricks p, UnexpectedMove),
            (isPlayersTurn (Player name) gs, NotPlayersTurn), (tricks >= 0, MoveAgainstRules "Tricks must be >= 0")]
            move gs

moveValidataion move@(TellColor name color) gs@GameState{phase=p} =
  checkMove [(isWaitingForColor p, UnexpectedMove),
            (isPlayersTurn (Player name) gs, NotPlayersTurn)] move gs


processMove :: PlayerMove -> GameState-> GameState
processMove (PlayCard name card) gs = gs{players = cardPlayedUpdate card (Player name) $ Model.players gs}
processMove (TellNumberOfTricks name tricks) gs =  gs{players = tricksPlayerUpdate tricks (Player name) $ Model.players gs}
processMove (TellColor _ color) gs = gs{currentColor=Just color}


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

playeableCards :: Player -> GameState -> Cards
playeableCards player gs@GameState{players=players, trump=trump, currentColor=currentColor} =
  fromMaybe [] $ do
    currentColor' <- currentColor
    player' <- find (\p -> player ==  (Model.player p)) players
    let table = cardsOnTable players
    let playersHand = hand player'
    let trumpsOnTable = cardsWithColor table trump
    let playersTrumps = cardsWithColor playersHand trump
    let playersCardsWithColor = cardsWithColor playersHand currentColor'
    case trumpsOnTable of
      (c:cs) -> Just $ orElseList playersTrumps (orElseList playersCardsWithColor playersHand)
      [] -> Just $ orElseList playersCardsWithColor playersHand

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

addPlayers :: [Player] -> GameState -> GameState
addPlayers newPlayers gs@GameState{players=players} = gs{players= players ++ (map initPlayerState newPlayers) }

isPlayersTurn :: Player -> GameState -> Bool
isPlayersTurn player GameState{phase=WaitingForCard player'}  = player == player'
isPlayersTurn player GameState{phase=WaitingForTricks player'}  =  player == player'
isPlayersTurn player GameState{phase=WaitingForColor player'}  =  player == player'

isWaitingForCards :: GamePhase -> Bool
isWaitingForCards (WaitingForCard _) = True
isWaitingForCards _ = False

isWaitingForTricks :: GamePhase -> Bool
isWaitingForTricks (WaitingForTricks  _) = True
isWaitingForTricks _ = False

isWaitingForColor :: GamePhase -> Bool
isWaitingForColor (WaitingForColor _) = True
isWaitingForColor _ = False


-- INIT --

initWildsowGameState :: StdGen -> GameState
initWildsowGameState gen =  GameState{
  phase = Idle,
  currentRound = 0,
  currentColor = Nothing,
  pile = [],
  trump = Gras,
  players= [],
  stdGen=gen
}


p1 = Player "Thomas Mueller"
p2 = Player "James Roriguez"
p3 = Player "Arjen Robben"
p4 = Player "Frank Ribery"
