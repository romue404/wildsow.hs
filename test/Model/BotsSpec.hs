module Model.BotsSpec where

import Test.HUnit

main :: IO ()
main = hspec spec

test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))
test2 = TestCase (do
                  (x,y) <- partA 3
                  assertEqual "for the first result of partA," 5 x
                  b <- partB y
                  assertBool ("(partB " ++ show y ++ ") failed") b)

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]