module Model.Logic (deal) where
import Model


deal :: Cards -> Int -> (Cards, Cards)
deal pile numberCards = (take numberCards pile, drop numberCards pile)


main :: IO ()
main = do
  print $ 1 / 2
