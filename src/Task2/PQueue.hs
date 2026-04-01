{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task2.PQueue (PQueue (..), Entry (..)) where

import Common.MonoidalTree
import Common.PriorityQueue
import Data.Function (on)
import Task1
import Task2.Tree
import Prelude hiding (max, min)

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

  insert k v = PQueue . (Entry (k, v) <|) . getTree

  extractMin :: forall k v. (Ord k) => PQueue k v -> Maybe (v, PQueue k v)
  extractMin = extract @(Min k)

  extractMax :: forall k v. (Ord k) => PQueue k v -> Maybe (v, PQueue k v)
  extractMax = extract @(Max k)

extract :: forall m k v. (Eq m, Measured m (Tree (MinMax k) (Entry k v)), Ord k) => PQueue k v -> Maybe (v, PQueue k v)
extract (PQueue tree) = (\(v, tree') -> (v, PQueue tree')) <$> go tree
  where
    next :: Tree (MinMax k) (Entry k v) -> Tree (MinMax k) (Entry k v) -> Bool
    next = on (==) (measure @m)

    go Empty = Nothing
    go (Leaf (Entry (_, v))) = Just (v, Empty)
    go t@(Branch _ l r)
      | next t l = (\(v, l') -> (v, fix1 (branch l' r))) <$> go l
      | next t r = (\(v, r') -> (v, fix1 (branch l r'))) <$> go r
      | otherwise = undefined
