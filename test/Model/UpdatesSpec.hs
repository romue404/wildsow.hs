module Model.UpdatesSpec where

import Test.Hspec
import Test.QuickCheck

import System.Random(StdGen, getStdGen, newStdGen, next, mkStdGen)
import Data.List

import Model.Updates
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
  describe "test playerorder" $ do
       it "test1" $ playerStates setNewRoundStarterTest `shouldBe` playerStates testSortedPlayers

-- setNewRoundStarterTest
setNewRoundStarterTest :: GameState
setNewRoundStarterTest =
  setNewRoundStarter gs
  where
  gs = testUnsortedPlayers


testUnsortedPlayers :: GameState
testUnsortedPlayers = (generateBasicGameState []){
  currentRound = 1,
  playerStates =
    [
      generateBasicPlayerState "TestBotName3" [],
      generateBasicPlayerState "TestBotName2" [],
      (generateBasicPlayerState "TestBotName1" []),
      (generateBasicPlayerState "TestBotName4" [])
    ]
}

testSortedPlayers :: GameState
testSortedPlayers = (generateBasicGameState []){
  currentRound = 1,
  playerStates =
    [
      generateBasicPlayerState "TestBotName1" [],
      generateBasicPlayerState "TestBotName2" [],
      (generateBasicPlayerState "TestBotName3" []),
      (generateBasicPlayerState "TestBotName4" [])
    ]
}



---- helper

-- Gamestate with all empty
generateOneCardGameState ::Cards -> GameState
generateOneCardGameState hand = (generateBasicGameState hand){
    trump = Gras,
    currentColor = Just Herz,
    pile = []
    }

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