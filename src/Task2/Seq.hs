{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task2.Seq (Seq (..), Elem (..)) where

import Common.MonoidalTree
import Common.Sequence
import Control.Applicative ((<|>))
import Task1
import Task2.Tree
import Prelude hiding (seq)

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

  (+|) x = Seq . (Elem x <|) . getTree

  xs |+ x = Seq . (|> Elem x) . getTree $ xs

  insertAt idx' a (Seq tree') = Seq (go idx' tree')
    where
      go idx tree | idx <= 0 = Elem a <| tree
      go idx tree | idx >= size tree = tree |> Elem a
      go idx (Branch _ l r)
        | idx <= size l = branch (go idx l) r
        | otherwise = branch l (go (idx - size l) r)
      go _ _ = undefined

  removeAt idx' (Seq tree') = Seq (go idx' tree')
    where
      go 0 (Leaf _) = Empty
      go idx (Branch _ l r)
        | idx < size l = fix1 $ branch (go idx l) r
        | otherwise = fix1 $ branch l (go (idx - size l) r)
      go _ tree = tree

  elemAt idx' (Seq tree') = go idx' tree'
    where
      go 0 (Leaf (Elem a)) = Just a
      go idx (Branch _ l r) = go idx l <|> go (idx - size l) r
      go _ _ = Nothing

size :: forall a. Tree (Size a) (Elem a) -> Int
size = getSize . (measure @(Size a))
