module Model.BotsSpec where

import Test.Hspec
import Test.QuickCheck

import System.Random(StdGen, getStdGen, newStdGen, next, mkStdGen)

import Model.Bots
import Model.Model

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
         it "add one" $ do
            addOneTest `shouldBe` addOneTestResult
         it "test playableCards2" $ do
            playableCards2Test `shouldBe` playableCards2TestResult

--testtest
addOneTest = aTest 41
addOneTestResult = 42

--
playableCards2Test :: Cards
playableCards2Test = playeableCards2 (trump gs) theCurrentColor (hand playerState1)
    where
    theCurrentColor = Herz
    theTrump = Gras
    theHand = [Card{value=Nine, color=Gras}, Card{value=Seven, color=Schellen}, Card{value=King, color=Gras}, Card{value=Ace, color=Herz}]
    playerState1 = PlayerState{
        player = RandomBot {playerName="TestBotName"},
        playedCard = Nothing,
        hand = theHand,
        tricks = [0],
        score = [0],
        tricksSubround =[(0,0)]
    }
    gs = GameState{
        phase = Idle,
        currentRound = 1,
        currentColor = Nothing,
        pile = [],
        trump = theTrump,
        playerStates= [playerState1],
        stdGen = mkStdGen 42
    }

playableCards2TestResult :: Cards
playableCards2TestResult = [Card{value=Ace, color=Herz}]