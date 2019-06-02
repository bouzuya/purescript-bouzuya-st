module Test.Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Test.Bouzuya.ST as ST
import Test.Bouzuya.ST.PriorityQueue as STPriorityQueue
import Test.Unit.Main as TestUnitMain

main :: Effect Unit
main = TestUnitMain.runTest do
  ST.tests
  STPriorityQueue.tests
