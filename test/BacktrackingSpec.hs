module BacktrackingSpec
  ( backtrackingTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import System.Random
import Control.Monad.State.Lazy
import Data.Set

import Sudoku.Backtracking


backtrackingTests = testGroup "backtracking tests"
  [ dieRollTests
  , choiceTests
  ]



sample :: Ord a => State StdGen a -> Int -> StdGen -> Set a
sample event size rng = fromList . take size $ evalState events rng
  where
    events = do
      first <- event
      rest <- events
      return (first:rest)



dieRollTests = testGroup "dieRoll"
  [ testCase "0-6" $ sample (dieRoll 6) 100 (mkStdGen 666) @?= fromList [0..5]
  , testCase "0-1" $ sample (dieRoll 1) 100 (mkStdGen 666) @?= singleton 0
  , testCase "0-0" $ sample (dieRoll 0) 100 (mkStdGen 666) @?= singleton 0
  ]


choiceTests = testGroup "choice"
  [ testCase "1..6" $ sample (choice [1..6]) 100 (mkStdGen 666) @?= fromList [1..6]
  ]
