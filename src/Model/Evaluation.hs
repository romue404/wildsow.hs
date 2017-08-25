module Model.Evaluation where
import Model.Model as Model
import Data.Ord(comparing)
import Data.List(maximumBy, find)
import Model.Updates (updatePlayer)
import Data.Maybe(catMaybes, fromMaybe)
import System.Random.Shuffle (shuffle')
import System.Random (StdGen, next)


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