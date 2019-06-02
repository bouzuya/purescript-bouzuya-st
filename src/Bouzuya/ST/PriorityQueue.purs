module Bouzuya.ST.PriorityQueue
  ( PriorityQueue
  , dequeue
  , empty
  , enqueue
  , fromArray
  , fromSTArray
  ) where

import Prelude

import Control.Monad.ST (ST, kind Region)
import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Partial.Unsafe as Unsafe

data PriorityQueue r a = PriorityQueue (STArray r a)

-- | extract maximum value
dequeue :: forall r a. Ord a => PriorityQueue r a -> ST r (Maybe a)
dequeue (PriorityQueue xs) = do
  maxMaybe <- STArray.shift xs
  minMaybe' <- STArray.pop xs
  case minMaybe' of
    Maybe.Nothing -> pure unit
    Maybe.Just x -> do
      _ <- STArray.unshift x xs
      _ <- update 0 xs
      pure unit
  pure maxMaybe

empty :: forall r a. Ord a => ST r (PriorityQueue r a)
empty = map PriorityQueue STArray.empty

enqueue :: forall r a. Ord a => a -> PriorityQueue r a -> ST r Unit
enqueue x (PriorityQueue xs) = do
  l <- STArray.push x xs
  iRef <- STRef.new (l - 1)
  ST.while
    do
      i <- STRef.read iRef
      pMaybe <- parent i xs
      case pMaybe of
        Maybe.Nothing -> pure false
        Maybe.Just (Tuple.Tuple _ p) -> do
          pure (x > p)
    do
      i <- STRef.read iRef
      pMaybe <- parent i xs
      case pMaybe of
        Maybe.Nothing -> Unsafe.unsafeCrashWith "no parent index"
        Maybe.Just (Tuple.Tuple pi _) -> do
          _ <- swap i pi xs
          _ <- STRef.write pi iRef
          pure unit

fromArray :: forall r a. Ord a => Array a -> ST r (PriorityQueue r a)
fromArray xs = (STArray.thaw xs) >>= fromSTArray

fromSTArray :: forall r a. Ord a => STArray r a -> ST r (PriorityQueue r a)
fromSTArray xs = do
  l <- map Array.length (STArray.unsafeFreeze xs)
  case parentIndex (l - 1) of
    Maybe.Nothing -> do
      pure (PriorityQueue xs)
    Maybe.Just pi -> do
      ST.for 0 (pi + 1) (\i -> update (pi - i) xs)
      pure (PriorityQueue xs)

-- private

parent :: forall r a. Int -> STArray r a -> ST r (Maybe (Tuple Int a))
parent i xs =
  case parentIndex i of
    Maybe.Nothing -> pure Maybe.Nothing
    Maybe.Just i' -> map (map (Tuple.Tuple i')) (STArray.peek i' xs)

parentIndex :: Int -> Maybe Int
parentIndex i
  | i <= 0 = Maybe.Nothing
  | otherwise = Maybe.Just ((i - 1) / 2)

leftChild :: forall r a. Int -> STArray r a -> ST r (Maybe (Tuple Int a))
leftChild i xs =
  let i' = (i * 2) + 1
  in map (map (Tuple.Tuple i')) (STArray.peek i' xs)

rightChild :: forall r a. Int -> STArray r a -> ST r (Maybe (Tuple Int a))
rightChild i xs =
  let i' = (i + 1) * 2
  in map (map (Tuple.Tuple i')) (STArray.peek i' xs)

update :: forall r a. Ord a => Int -> STArray r a -> ST r Unit
update i xs = do
  xs' <- STArray.unsafeFreeze xs
  lMaybe <- leftChild i xs
  rMaybe <- rightChild i xs
  let
    x = Unsafe.unsafePartial (Array.unsafeIndex xs' i)
    maxI =
      case lMaybe, rMaybe of
        (Maybe.Just (Tuple.Tuple li l)), (Maybe.Just (Tuple.Tuple ri r)) ->
          if x > l
            then if x > r then i else ri
            else if l > r then li else ri
        (Maybe.Just (Tuple.Tuple li l)), Maybe.Nothing ->
          if x > l then i else li
        Maybe.Nothing, (Maybe.Just (Tuple.Tuple ri r)) ->
          if x > r then i else ri
        Maybe.Nothing, Maybe.Nothing -> i
  if maxI /= i
    then do
      _ <- swap i maxI xs
      update maxI xs
    else pure unit

swap :: forall r a. Int -> Int -> STArray r a -> ST r Boolean
swap i j xs = do
  xMaybe <- STArray.peek i xs
  yMaybe <- STArray.peek j xs
  case xMaybe, yMaybe of
    (Maybe.Just x), (Maybe.Just y) -> do
      _ <- STArray.poke j x xs
      _ <- STArray.poke i y xs
      pure true
    _, _ -> pure false
