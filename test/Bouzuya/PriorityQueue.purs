module Test.Bouzuya.ST.PriorityQueue
  ( tests
  ) where

import Prelude

import Bouzuya.ST.PriorityQueue (PriorityQueue)
import Bouzuya.ST.PriorityQueue as PriorityQueue
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Foldable as Foldable
import Data.Maybe as Maybe
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "Bouzuya.ST.PriorityQueue" do
  TestUnit.test "((empty + enqueue) / fromArray) -> dequeue" do
    let
      enqueue' :: Array Int -> Array Int
      enqueue' xs = ST.run do
        q <- PriorityQueue.empty
        ST.foreach xs \x -> do
          PriorityQueue.enqueue x q
        toSortedArray q

      fromArray' :: Array Int -> Array Int
      fromArray' xs = ST.run ((PriorityQueue.fromArray xs) >>= toSortedArray)

      toSortedArray :: forall r. PriorityQueue r Int -> ST r (Array Int)
      toSortedArray q = do
        sta <- STArray.empty
        ST.while
          do
            xMaybe <- PriorityQueue.dequeue q
            case xMaybe of
              Maybe.Nothing -> pure false
              Maybe.Just x -> do
                _ <- STArray.push x sta
                pure true
          (pure unit)
        STArray.unsafeFreeze sta

    Foldable.for_
      [ []
      , [1]
      , [1, 2]
      , [2, 1]
      , [1, 2, 3, 4, 5]
      ]
      \xs -> do
        let sorted = Array.sortBy (flip compare) xs
        Assert.equal sorted (fromArray' xs)
        Assert.equal sorted (enqueue' xs)
