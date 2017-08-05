{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-} -- realtions between types
 module Types where

class CardEq a where
  colorEq :: a -> a -> Bool
  valueEq :: a -> a -> Bool
  completeEq :: a -> a -> Bool
  completeEq a b = colorEq a b && valueEq a b

