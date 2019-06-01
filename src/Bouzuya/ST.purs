module Bouzuya.ST
  ( foreachWithIndex
  ) where

import Prelude

import Control.Monad.ST (ST)

foreign import foreachWithIndex
  :: forall r a. Array a -> (Int -> a -> ST r Unit) -> ST r Unit
