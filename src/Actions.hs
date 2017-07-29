module Actions where
import Model
import System.Random


deal :: Cards -> Int -> (Cards, Cards)
deal pile numberCards = (take numberCards pile, drop numberCards pile)

shuffle :: [a] -> IO[a]
shuffle x = if length x < 2 then return x else do
	i <- System.Random.randomRIO (0, length(x)-1)
	r <- shuffle (take i x ++ drop (i+1) x)
	return (x!!i : r)


