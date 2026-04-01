{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task3.Tree where

import Common.MonoidalTree
import Task1 (Measured (..))

-- * 2-3 tree definition

-- | 2-3 tree with values 'a' in leaves
-- Intermediate nodes contain only accumulated measure 'm'
data Tree m a
  = Empty
  | Leaf a
  | Node2 m (Tree m a) (Tree m a)
  | Node3 m (Tree m a) (Tree m a) (Tree m a)
  | Node4 (Tree m a) (Tree m a) (Tree m a) (Tree m a)
  deriving (Show, Eq)

-- | Measures given tree using provided measure of 'a'
instance (Measured m a) => Measured m (Tree m a) where
  measure Empty = mempty
  measure (Leaf a) = measure a
  measure (Node2 m _ _) = m
  measure (Node3 m _ _ _) = m
  measure (Node4 _ _ _ _) = error "Node4 should be temporary"

instance Foldable (Tree m) where
  foldMap f = go
    where
      go Empty = mempty
      go (Leaf a) = f a
      go (Node2 _ a b) = go a <> go b
      go (Node3 _ a b c) = go a <> go b <> go c
      go (Node4 _ _ _ _) = error "Node4 should be temporary"

-- * Smart constructors

leaf :: a -> Tree m a
leaf = Leaf

node2 :: (Measured m a) => Tree m a -> Tree m a -> Tree m a
node2 a b = Node2 (foldMap measure [a, b]) a b

node3 :: (Measured m a) => Tree m a -> Tree m a -> Tree m a -> Tree m a
node3 a b c = Node3 (foldMap measure [a, b, c]) a b c

-- * Monoidal tree instance

instance MonoidalTree Tree where
  toTree = foldr (<|) Empty
  (<|) x = fix1 . go
    where
      go Empty = leaf x
      go l@(Leaf _) = node2 (leaf x) l
      go (Node2 _ a b) = fix1 $ node2 (go a) b
      go (Node3 _ a b c) = fix1 $ node3 (go a) b c
      go (Node4 _ _ _ _) = error "Node4 should be temporary"
  tree |> x = (fix1 . go) tree
    where
      go Empty = leaf x
      go l@(Leaf _) = node2 l (leaf x)
      go (Node2 _ a b) = fix1 $ node2 a (go b)
      go (Node3 _ a b c) = fix1 $ node3 a b (go c)
      go (Node4 _ _ _ _) = error "Node4 should be temporary"

fix1 :: (Measured m a) => Tree m a -> Tree m a
-- Leaf Node2
fix1 (Node2 _ (Node2 _ a b) c@(Leaf _)) = node3 a b c
fix1 (Node2 _ a@(Leaf _) (Node2 _ b c)) = node3 a b c
-- Leaf Node3
fix1 (Node3 _ (Node2 _ a b) c@(Leaf _) d@(Leaf _)) = Node4 a b c d
fix1 (Node3 _ a@(Leaf _) (Node2 _ b c) d@(Leaf _)) = Node4 a b c d
fix1 (Node3 _ a@(Leaf _) b@(Leaf _) (Node2 _ c d)) = Node4 a b c d
-- Node2 with Node4 child
fix1 (Node2 _ (Node2 _ a b) (Node4 c d e f)) = node2 (node3 a b c) (node3 d e f)
fix1 (Node2 _ (Node3 _ a b c) (Node4 d e f g)) = node3 (node2 a b) (node2 c d) (node3 e f g)
fix1 (Node2 _ (Node4 a b c d) (Node2 _ e f)) = node2 (node3 a b c) (node3 d e f)
fix1 (Node2 _ (Node4 a b c d) (Node3 _ e f g)) = node3 (node2 a b) (node2 c d) (node3 e f g)
-- Node3 with Node4 child
fix1 (Node3 _ (Node2 _ a b) (Node2 _ c d) (Node4 e f g h)) = node3 (node2 a b) (node3 c d e) (node3 f g h)
fix1 (Node3 _ (Node2 _ a b) (Node3 _ c d e) (Node4 f g h i)) = node3 (node3 a b c) (node3 d e f) (node3 g h i)
fix1 (Node3 _ (Node2 _ a b) (Node4 c d e f) (Node2 _ g h)) = node3 (node2 a b) (node3 c d e) (node3 f g h)
fix1 (Node3 _ (Node2 _ a b) (Node4 c d e f) (Node3 _ g h i)) = node3 (node3 a b c) (node3 d e f) (node3 g h i)
fix1 (Node3 _ (Node3 _ a b c) (Node2 _ d e) (Node4 f g h i)) = node3 (node3 a b c) (node3 d e f) (node3 g h i)
fix1 (Node3 _ (Node3 _ a b c) (Node3 _ d e f) (Node4 g h i j)) = Node4 (node2 a b) (node2 c d) (node3 e f g) (node3 h i j)
fix1 (Node3 _ (Node3 _ a b c) (Node4 d e f g) (Node2 _ h i)) = node3 (node3 a b c) (node3 d e f) (node3 g h i)
fix1 (Node3 _ (Node3 _ a b c) (Node4 d e f g) (Node3 _ h i j)) = Node4 (node2 a b) (node2 c d) (node3 e f g) (node3 h i j)
fix1 (Node3 _ (Node4 a b c d) (Node2 _ e f) (Node2 _ g h)) = node3 (node2 a b) (node3 c d e) (node3 f g h)
fix1 (Node3 _ (Node4 a b c d) (Node2 _ e f) (Node3 _ g h i)) = node3 (node3 a b c) (node3 d e f) (node3 g h i)
fix1 (Node3 _ (Node4 a b c d) (Node3 _ e f g) (Node2 _ h i)) = node3 (node3 a b c) (node3 d e f) (node3 g h i)
fix1 (Node3 _ (Node4 a b c d) (Node3 _ e f g) (Node3 _ h i j)) = Node4 (node2 a b) (node2 c d) (node3 e f g) (node3 h i j)
---- Top-level Node4
fix1 (Node4 a b c d) = node2 (node2 a b) (node2 c d)
----
fix1 tree = tree

-- leafDepths :: Int -> Tree m a -> [Int]
-- leafDepths _ Empty = []
-- leafDepths d (Leaf _) = [d]
-- leafDepths d (Node2 _ l r) = leafDepths (d + 1) l ++ leafDepths (d + 1) r
-- leafDepths d (Node3 _ l m r) = leafDepths (d + 1) l ++ leafDepths (d + 1) m ++ leafDepths (d + 1) r

-- instance {-# INCOHERENT #-} Measured () a where
--   measure = const ()
