module ModelUpdates where

import Model
import Data.List
import Data.Ord
import Data.Maybe
import Control.Monad
import Types
import System.Random.Shuffle
import System.Random


-- data GamePhase = NewGame | GameOver | WaitingForTricks Player | WaitingForCards Player   | Evaluation
step :: GameState -> GameState
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
step gs@GameState {phase = _} = gs


processMove :: PlayerMove -> GameState-> GameState
processMove (PlayCard player card) gs =  gs{players = cardPlayedUpdate card player $ Model.players gs}
processMove (TellNumberOfTricks player tricks) gs =  gs{players = tricksPlayerUpdate tricks player $ Model.players gs}
processMove (TellColor _ color) gs = gs{currentColor=Just color}


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

-- show this in presentation
dealCards :: GameState -> GameState
dealCards gs@GameState{currentRound=round, pile=pile, players=players, stdGen = gen} =
  gs{pile=undealtCards, players=playersWithDealtCards, stdGen=gen'}
    where numberOfCards = (cardsPerRound Model.deck $ length players) !! round
          (shuffledDeck, gen') = (shuffle' Model.deck (length Model.deck) gen, snd $ next gen)
          chunked = sublist numberOfCards shuffledDeck
          playersWithDealtCards = map (\(chunk, player) ->player{hand=chunk, playedCard=Nothing}) (zip chunked players)
          undealtCards = concat $ drop (length players) chunked


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

isPlayersTurn :: Player -> GameState -> Bool
isPlayersTurn player gameState =  player == (Model.player $ head $ players gameState)

nextPlayer :: [PlayerState] -> [PlayerState]
nextPlayer (p:ps) = ps ++ [p]

allTricksSet :: GameState -> Bool
allTricksSet gameState =  flip(all) players' haveEnoughEntries
  where players' = players gameState
        round = currentRound gameState
        haveEnoughEntries = (\PlayerState{tricks=t} -> length(t) >=  round)





-- INIT --

initWildsowGameState :: StdGen -> GameState
initWildsowGameState gen = dealCards .setNewTrump $ GameState{
  phase = Idle,
  currentRound = 0,
  currentColor = Nothing,
  pile = [],
  trump = Gras,
  players=(initPlayerState p1):(initPlayerState p2):(initPlayerState p3):(initPlayerState p4):[],
  stdGen=gen
}

initPlayerState player = PlayerState player Nothing [] [] [] []
p1 = Player "Thomas Mueller" 1
p2 = Player "James Roriguez" 2
p3 = Player "Arjen Robben" 3
p4 = Player "Frank Ribery" 4
