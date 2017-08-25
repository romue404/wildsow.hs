{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

 module Model.Types where

class CardEq a where
  colorEq :: a -> a -> Bool
  valueEq :: a -> a -> Bool
  completeEq :: a -> a -> Bool
  completeEq a b = colorEq a b && valueEq a b

class PlayerAction action where
  whos :: action -> String
