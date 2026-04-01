{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task3.PQueue where

import Common.MonoidalTree (MonoidalTree (..))
import Common.PriorityQueue
import Task1 (Measured (..), MinMax (..))
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

  extractMin = error "TODO: define extractMin (PriorityQueue Task3.PQueue)"
  extractMax = error "TODO: define extractMax (PriorityQueue Task3.PQueue)"
