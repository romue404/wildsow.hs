module Actions where
import Model
import System.Random


-- function withResupply - endless stream of shuffled decks
-- needs work when not enough cards can be dealt
deal :: Cards -> Int -> (Cards, Cards)
deal pile numberCards = (take numberCards pile, drop numberCards pile)

drawTrump :: Cards -> (Card, Cards)
drawTrump [] = (head deck, tail deck)
drawTrump (trump : rest) = (trump, rest)

shuffle :: [a] -> IO[a]
shuffle x = if length x < 2 then return x else do
  i <- System.Random.randomRIO (0, length(x)-1)
  r <- shuffle (take i x ++ drop (i+1) x)
  return (x!!i : r)

maxCardsPerPlayer :: Cards -> Int -> Int
maxCardsPerPlayer deck numberOfPlayers =  (length deck) `div` numberOfPlayers

nextPlayerNumber :: Int -> Int -> Int
nextPlayerNumber previousPlayerNumber numberOfPlayers = previousPlayerNumber `mod` numberOfPlayers + 1

cardsPerRound :: Cards -> Int -> [Int]
cardsPerRound deck players =
  map (\x -> abs(x - rounds) + 1) [1..rounds * 2 - 1]
  where rounds = maxCardsPerPlayer deck players

cardsWithColor :: Cards -> Color -> Cards
cardsWithColor hand color = filter (\(Card v c) ->  c == color) hand

allowedToPlay :: Cards -> Color -> Color -> Cards
allowedToPlay hand trumpColor currentColor =
  case cardsWithColor hand trumpColor of
       trumpCards@(c:cs)-> trumpCards
       [] -> case cardsWithColor hand currentColor of
                  currentColorCards@(cc:ccs) -> currentColorCards
                  [] -> hand



