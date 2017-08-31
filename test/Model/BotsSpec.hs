module Model.BotsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import System.Random(StdGen, newStdGen, next, mkStdGen)

import Model.Bots
import Model.Model

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

main2 :: IO ()
main2 = do
    gen <- newStdGen
    hspec $ spec2 gen

spec :: Spec
spec = do
    describe "aTest" $ do
        it "add one" $ do
        aTest 41 `shouldBe` 42
--    it "is idempotent" $ property $
--      \a -> aTest a === aTest (aTest a)

spec2 :: StdGen -> Spec
spec2 gen= do
    describe "playableCards2" $ do
        it "test playableCards2..." $ do
        playeableCards2 (trump gs) theCurrentColor (hand playerState1) `shouldBe` result
        where
        theCurrentColor = Gras
        theTrump = Gras
        theHand = [Card{value=Nine, color=Herz}, Card{value=Seven, color=Herz}, Card{value=King, color=Herz}, Card{value=Ace, color=Herz}]
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
            stdGen=gen
        }
        result = [Card{value=Nine, color=Herz}, Card{value=Seven, color=Herz}, Card{value=King, color=Herz}, Card{value=Ace, color=Herz}]