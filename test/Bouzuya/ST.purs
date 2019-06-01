module Test.Bouzuya.ST
  ( tests
  ) where

import Prelude

import Bouzuya.ST as BouzuyaST
import Control.Monad.ST as ST
import Data.Array.ST as STArray
import Data.Tuple as Tuple
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "" do
  TestUnit.test "foreachWithIndex" do
    let
      xs = ["a", "b", "c"]
      xs' =
        ST.run do
          sta <- STArray.empty
          BouzuyaST.foreachWithIndex xs \i x -> do
            void (STArray.push (Tuple.Tuple i x) sta)
          STArray.unsafeFreeze sta
    Assert.equal [Tuple.Tuple 0 "a", Tuple.Tuple 1 "b", Tuple.Tuple 2 "c"] xs'
