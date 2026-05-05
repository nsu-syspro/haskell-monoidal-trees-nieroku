{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task3.PQueue where

import Common.MonoidalTree (MonoidalTree (..))
import Common.PriorityQueue
import Data.Function (on)
import Task1 (Max (..), Measured (..), Min (..), MinMax (..))
import Task3.Tree

-- * Priority queue definition

-- | Priority queue based on binary tree
newtype PQueue k v = PQueue {getTree :: Tree (MinMax k) (Entry k v)}
  deriving (Show, Eq)

-- | Priority queue entry wrapper
newtype Entry k v = Entry {getEntry :: (k, v)}
  deriving (Show, Eq)

instance (Ord k) => Measured (MinMax k) (Entry k v) where
  measure = measure . fst . getEntry

-- * Priority queue instance

instance PriorityQueue PQueue where
  empty = PQueue Empty

  entries = (foldr ((:) . getEntry) []) . getTree

  insert k v (PQueue tree') = PQueue (Entry (k, v) <| tree')

  extractMin :: forall k v. (Ord k) => PQueue k v -> Maybe (v, PQueue k v)
  extractMin = extract @(Min k)

  extractMax :: forall k v. (Ord k) => PQueue k v -> Maybe (v, PQueue k v)
  extractMax = extract @(Max k)

extract :: forall m k v. (Eq m, Measured m (Tree (MinMax k) (Entry k v)), Ord k) => PQueue k v -> Maybe (v, PQueue k v)
extract (PQueue tree) = (\(v, tree') -> (v, PQueue (finishDeletion tree'))) <$> go tree
  where
    next :: Tree (MinMax k) (Entry k v) -> Tree (MinMax k) (Entry k v) -> Bool
    next = on (==) (measure @m)

    adjust = fmap . fmap

    go Empty = Nothing
    go (Leaf (Entry (_, v))) = Just (v, pure Empty)
    go t@(Node2 _ a b)
      | next t a = (\a' -> delete2 a' (pure b)) `adjust` go a
      | next t b = (\b' -> delete2 (pure a) b') `adjust` go b
      | otherwise = undefined
    go t@(Node3 _ a b c)
      | next t a = (\a' -> delete3 a' (pure b) (pure c)) `adjust` go a
      | next t b = (\b' -> delete3 (pure a) b' (pure c)) `adjust` go b
      | next t c = (\c' -> delete3 (pure a) (pure b) c') `adjust` go c
      | otherwise = undefined
