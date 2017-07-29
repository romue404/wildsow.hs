module Actions where
import Model


deal :: Cards -> Int -> (Cards, Cards)
deal pile numberCards = (take numberCards pile, drop numberCards pile)
