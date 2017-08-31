module Model.BotsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Model.Bots

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "aTest" $ do
    it "add one" $ do
      aTest 41 `shouldBe` 42
--    it "is idempotent" $ property $
--      \a -> aTest a === aTest (aTest a)