{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task3.Seq (Seq (..), Elem (..)) where

import Common.MonoidalTree (MonoidalTree (..))
import Common.Sequence
import Control.Applicative ((<|>))
import Task1 (Measured (..), Size (..))
import Task3.Tree

-- * Sequence definition

-- | Random-access sequence based on binary tree
newtype Seq a = Seq {getTree :: Tree (Size a) (Elem a)}
  deriving (Show, Eq)

-- | Sequence element wrapper
newtype Elem a = Elem {getElem :: a}
  deriving (Show, Eq)

-- | Measures given element as 'Size 1'
instance Measured (Size a) (Elem a) where
  measure = const (Size 1)

instance Foldable Seq where
  foldMap f = (foldMap (f . getElem)) . getTree

  -- An O(1) implementation of length is possible
  -- due to size of the tree being cached at each node
  length :: forall a. Seq a -> Int
  length = size . getTree

-- * Sequence instance

instance Sequence Seq where
  empty = Seq Empty
  x +| xs = Seq (Elem x <| getTree xs)
  xs |+ x = Seq (getTree xs |> Elem x)

  insertAt idx' x' (Seq tree') = Seq (fix1 (go idx' tree'))
    where
      x = Elem x'

      go idx tree
        | idx < 0 = go 0 tree
        | idx > size tree = go (size tree) tree
      go 0 Empty = leaf x
      go 0 b@(Leaf _) = node2 (leaf x) b
      go 1 a@(Leaf _) = node2 a (leaf x)
      go idx (Node2 _ a b)
        | idx <= size a = fix1 $ node2 (go idx a) b
        | otherwise = fix1 $ node2 a (go (idx - size a) b)
      go idx (Node3 _ a b c)
        | idx <= size a = fix1 $ node3 (go idx a) b c
        | idx - size a <= size b = fix1 $ node3 a (go (idx - size a) b) c
        | otherwise = fix1 $ node3 a b (go (idx - size a - size b) c)
      go _ _ = undefined

  removeAt idx' (Seq tree') = Seq (fix1 (go idx' tree'))
    where
      go 0 (Leaf _) = Empty
      go idx (Node2 _ a b)
        | idx < size a = fix1 $ node2 (go idx a) b
        | otherwise = fix1 $ node2 a (go (idx - size a) b)
      go idx (Node3 _ a b c)
        | idx < size a = fix1 $ node3 (go idx a) b c
        | idx - size a < size b = fix1 $ node3 a (go (idx - size a) b) c
        | otherwise = fix1 $ node3 a b (go (idx - size a - size b) c)
      go _ tree = tree

  elemAt idx' (Seq tree') = go idx' tree'
    where
      go idx tree | idx < 0 || size tree <= idx = Nothing
      go _ Empty = Nothing
      go 0 (Leaf (Elem x)) = Just x
      go idx (Node2 _ a b) = go idx a <|> go (idx - size a) b
      go idx (Node3 _ a b c) = go idx a <|> go (idx - size a) b <|> go (idx - size a - size b) c
      go _ _ = undefined

size :: forall a. Tree (Size a) (Elem a) -> Int
size = getSize . (measure @(Size a))
