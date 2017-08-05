module ModelUpdates where

import Model
import Data.List
import Data.Ord
import Data.Maybe
import Control.Monad
import Types


-- data GamePhase = NewGame | GameOver | WaitingForTricks Player | WaitingForCards Player   | Evaluation
step :: GameState -> GameState
step gs@GameState {phase = WaitingForTricks p} = gs{players=stepQueue, phase=tricksOrCards}
  where players' = players gs
        stepQueue = nextPlayer players'
        next = player $ head stepQueue
        tricksOrCards = if allTricksSet gs
                        then WaitingForCard next
                        else WaitingForTricks next
step gs@GameState {phase = WaitingForCard p} = if everyPlayerPlayed gs then step gs{phase=Evaluation}
                                                else waitForNextCard gs

step gs@GameState {phase=WaitingForColor p} = case currentColor gs of
                                                Nothing -> gs
                                                Just c -> gs{phase = WaitingForTricks p}
step gs@GameState {phase = Evaluation} = if not $ allHandsPlayed gs
                                         then (clearPlayedCards . setNewTrump . waitForNextCard . evaluateSubRound) gs
                                         else waitForNextCard gs -- TODO
step gs@GameState {phase = GameOver} = gs




processMove :: PlayerMove -> GameState-> GameState
processMove (PlayCard player card) gs =  gs{players = cardPlayedUpdate card player $ Model.players gs}
processMove (TellNumberOfTricks player tricks) gs =  gs{players = tricksPlayerUpdate tricks player $ Model.players gs}
processMove (TellColor _ color) gs = gs{currentColor=Just color}


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


highestCard :: [(Player, Card)] -> Player
highestCard pcs = fst$ maximumBy (comparing(value . snd)) pcs

waitForNextCard :: GameState-> GameState
waitForNextCard gameState =  gameState{players = nextPlayer(players gameState), phase=WaitingForCard (player $ head $  nextPlayer $ players gameState)}

clearPlayedCards :: GameState -> GameState
clearPlayedCards gameState =
  let players = Model.players gameState
      players' = map (\ps -> ps{playedCard=Nothing}) players
  in gameState {players = players'}

setNewTrump :: GameState -> GameState
setNewTrump gameState = gameState {trump= Model.color . head $ pile gameState, pile= tail $ pile gameState}

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
        haveEnoughEntries = (\p -> length(tricks p) == round)
