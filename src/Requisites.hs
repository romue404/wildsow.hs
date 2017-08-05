module Requisites where

listAll :: (Enum a, Bounded a) => [a]
listAll = [minBound .. maxBound]

