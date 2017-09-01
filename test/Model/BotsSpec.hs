module Model.BotsSpec where

import Test.Hspec
import Test.QuickCheck

import System.Random(StdGen, getStdGen, newStdGen, next, mkStdGen)
import Data.List

import Model.Bots
import Model.Model
import Model.Validation

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
{-
main :: IO ()
main = hspec spec
-}

main :: IO ()
main = hspec $ spec

--  hspec spec


--spec :: StdGen -> Spec
spec :: Spec
spec = do
  describe "botSpecTest" $ do
       it "add one" $
          addOneTest `shouldBe` 42
  describe "test playableCards2" $ do
       it "Currentcolor and Trump -> CurrentColor" $
          playableCards2Test Gras Herz [Card Nine Gras, Card Seven Schellen, Card King Gras, Card Ace Herz]
          `shouldBe` [Card Ace Herz]
       it "Trump and No CurrentColor -> Trump" $
          playableCards2Test Gras Herz [Card Nine Eichel, Card Seven Schellen, Card King Gras]
          `shouldBe` [Card King Gras]
       it "No card fits -> all" $
          playableCards2Test Gras Herz [Card Nine Schellen, Card Seven Eichel, Card King Schellen, Card Ace Eichel]
          `shouldBe` [Card Nine Schellen, Card Seven Eichel, Card King Schellen, Card Ace Eichel]
  describe "test possibleHigherCards" $ do
       it "no currentcolor, higer trump" $
          possibleHigherCardsTest Nothing (Card King Gras) [Card King Gras, Card Eight Schellen]
          `shouldBe` [Card Ten Gras]
       it "no currentcolor, no trump -> higher color and all trumps" $
          possibleHigherCardsTest Nothing (Card King Schellen) [Card King Eichel, Card Eight Schellen]
          -- remove the pile
          `shouldBe` delete (Card Ace Gras) (filter (\Card{value=v', color=c'} ->  c' == Gras) deck)  ++ [Card Ten Schellen, Card Ace Schellen]
       it "currentcolor, higer trump" $
          possibleHigherCardsTest (Just Herz) (Card King Herz) [Card King Herz, Card Eight Schellen]
          `shouldBe` delete (Card Ace Gras) (filter (\Card{value=v', color=c'} ->  c' == Gras) deck) ++ [Card Ten Herz, Card Ace Herz]
       it "currentcolor, higer trump, without own hand" $
          possibleHigherCardsTest (Just Herz) (Card King Herz) [Card King Herz, Card Eight Schellen, Card Ace Herz]
          `shouldBe` delete (Card Ace Gras) (filter (\Card{value=v', color=c'} ->  c' == Gras) deck) ++ [Card Ten Herz]
       -- TODO it "currentcolor, higer trump, without own hand, without played cards" $
  describe "test opponentPlayedCards" $ do
       it "just two cards" $
          opponentPlayedCardsTest
          `shouldBe` [Card Jack Gras, Card Queen Gras]
  describe "test cardWinningChance" $ do
       it "only trump" $
          cardWinningChanceTrumpTest (Card Ten Gras)
          `shouldBe` 1.0
       it "with currentColor" $
          cardWinningChanceTest (Card Ten Gras)
          `shouldBe` 1.0

--testtest
addOneTest = aTest 41

-- playableCards2Test
playableCards2Test :: Color -> Color -> Cards -> Cards
playableCards2Test trump currentColor hand =
  playeableCards2 trump currentColor hand
  where
    gs = (generateBasicGameState hand){
      trump = trump,
      currentColor = Just currentColor
    }
    ps = head $ playerStates gs

-- possibleHigherCards
possibleHigherCardsTest :: Maybe Color -> Card -> Cards -> Cards
possibleHigherCardsTest currentColor card hand = possibleHigherCards gs ps card
  where
    gs = (generateBasicGameState hand){
      trump = Gras,
      currentColor = currentColor,
      pile = [Card Ace Gras]
    }
    ps = head $ playerStates gs

-- cardWinningChance
cardWinningChanceTrumpTest :: Card -> Double
cardWinningChanceTrumpTest card = cardWinningChance gs card
  where
    gs = (generateBasicGameState []){
      trump = Gras,
      currentColor = Just Gras,
      pile = [Card Ace Gras]
    }

opponentPlayedCardsTest :: Cards
opponentPlayedCardsTest = opponentPlayedCards gs
  where
    gs = (generateBasicGameState []){
      trump = Gras,
      currentColor = Just Gras,
      pile = [Card Ace Gras],
      playerStates =
        [
          generateBasicPlayerState "TestBotName1" [],
          generateBasicPlayerState "TestBotName2" [],
          (generateBasicPlayerState "TestBotName3" []){
            playedCard = Just $ Card Jack Gras
          },
          (generateBasicPlayerState "TestBotName4" []){
            playedCard = Just $ Card Queen Gras
          }
        ]
    }


cardWinningChanceTest :: Card -> Double
cardWinningChanceTest card = cardWinningChance gs card
  where
    gs = (generateBasicGameState []){
      trump = Gras,
      currentColor = Just Herz,
      pile = [Card Ace Gras]
    }


---- helper

-- GameState with one Player
generateBasicGameState :: Cards -> GameState
generateBasicGameState hand = gs
  where
    playerState1 = generateBasicPlayerState "TestBotName1" hand
    gs = GameState{
      phase = Idle,
      currentRound = 1,
      currentColor = Just Herz,
      pile = [],
      trump = Gras,
      playerStates= [playerState1],
      stdGen = mkStdGen 42
    }

generateBasicPlayerState :: String -> Cards -> PlayerState
generateBasicPlayerState playerName hand = PlayerState{
    player = RandomBot {playerName=playerName},
    playedCard = Nothing,
    hand = hand,
    tricks = [0],
    score = [0],
    tricksSubround =[(0,0)]
}