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
  deriving (Show, Eq)

-- | Measures given tree using provided measure of 'a'
instance (Measured m a) => Measured m (Tree m a) where
  measure Empty = mempty
  measure (Leaf a) = measure a
  measure (Node2 m _ _) = m
  measure (Node3 m _ _ _) = m

instance Foldable (Tree m) where
  foldMap f = go
    where
      go Empty = mempty
      go (Leaf a) = f a
      go (Node2 _ a b) = go a <> go b
      go (Node3 _ a b c) = go a <> go b <> go c

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
  x <| tree = (finishInsertion . go) tree
    where
      go Empty = pure (leaf x)
      go l@(Leaf _) = pure (node2 (leaf x) l)
      go (Node2 _ a b) = insert2 (go a) (pure b)
      go (Node3 _ a b c) = insert3 (go a) (pure b) (pure c)
  tree |> x = (finishInsertion . go) tree
    where
      go Empty = pure (leaf x)
      go l@(Leaf _) = pure (node2 l (leaf x))
      go (Node2 _ a b) = insert2 (pure a) (go b)
      go (Node3 _ a b c) = insert3 (pure a) (pure b) (go c)

-- * Insertion

type InsertionResult m a = Either ((Tree m a), (Tree m a), (Tree m a), (Tree m a)) (Tree m a)

insertionSubtrees :: InsertionResult m a -> [Tree m a]
insertionSubtrees (Right (Node2 _ a b)) = [a, b]
insertionSubtrees (Right (Node3 _ a b c)) = [a, b, c]
insertionSubtrees (Left (a, b, c, d)) = [a, b, c, d]
insertionSubtrees _ = undefined

insert2 :: (Measured m a) => InsertionResult m a -> InsertionResult m a -> InsertionResult m a
--
insert2 (Right a@(Leaf _)) (Right (Node2 _ b c)) = Right $ node3 a b c
insert2 (Right (Node2 _ a b)) (Right c@(Leaf _)) = Right $ node3 a b c
--
insert2 (Right a) (Right b) = Right $ node2 a b
--
insert2 x y = case concatMap insertionSubtrees [x, y] of
  [a, b, c, d, e, f] -> Right $ (node2 (node3 a b c) (node3 d e f))
  [a, b, c, d, e, f, g] -> Right $ (node3 (node2 a b) (node2 c d) (node3 e f g))
  _ -> undefined

insert3 :: (Measured m a) => InsertionResult m a -> InsertionResult m a -> InsertionResult m a -> InsertionResult m a
--
insert3 (Right (Node2 _ a b)) (Right c@(Leaf _)) (Right d@(Leaf _)) = Left (a, b, c, d)
insert3 (Right a@(Leaf _)) (Right (Node2 _ b c)) (Right d@(Leaf _)) = Left (a, b, c, d)
insert3 (Right a@(Leaf _)) (Right b@(Leaf _)) (Right (Node2 _ c d)) = Left (a, b, c, d)
--
insert3 (Right a) (Right b) (Right c) = Right $ node3 a b c
--
insert3 (Left (a, b, c, d)) (Right (Node2 _ e f)) (Right z) = Right $ node3 (node3 a b c) (node3 d e f) z
insert3 (Right (Node2 _ a b)) (Left (c, d, e, f)) (Right z) = Right $ node3 (node3 a b c) (node3 d e f) z
insert3 (Right x) (Left (a, b, c, d)) (Right (Node2 _ e f)) = Right $ node3 x (node3 a b c) (node3 d e f)
insert3 (Right x) (Right (Node2 _ a b)) (Left (c, d, e, f)) = Right $ node3 x (node3 a b c) (node3 d e f)
insert3 x y z = case concatMap insertionSubtrees [x, y, z] of
  [a, b, c, d, e, f, g, h, i] -> Right $ node3 (node3 a b c) (node3 d e f) (node3 g h i)
  [a, b, c, d, e, f, g, h, i, j] -> Left (node2 a b, node2 c d, node3 e f g, node3 h i j)
  _ -> undefined
  where

finishInsertion :: (Measured m a) => InsertionResult m a -> Tree m a
finishInsertion (Right tree) = tree
finishInsertion (Left (a, b, c, d)) = node2 (node2 a b) (node2 c d)

-- * Deletion

type DeletionResult m a = Either (Tree m a) (Tree m a)

deletionSubtrees :: DeletionResult m a -> [Tree m a]
deletionSubtrees (Right (Node2 _ a b)) = [a, b]
deletionSubtrees (Right (Node3 _ a b c)) = [a, b, c]
deletionSubtrees (Left a) = [a]
deletionSubtrees _ = undefined

delete2 :: (Measured m a) => DeletionResult m a -> DeletionResult m a -> DeletionResult m a
--
delete2 (Right Empty) (Right a) = Left a
delete2 (Right a) (Right Empty) = Left a
--
delete2 (Right a) (Right b) = Right $ node2 a b
--
delete2 x y = case concatMap deletionSubtrees [x, y] of
  [a, b, c] -> Left $ node3 a b c
  [a, b, c, d] -> Right $ node2 (node2 a b) (node2 c d)
  _ -> undefined

delete3 :: (Measured m a) => DeletionResult m a -> DeletionResult m a -> DeletionResult m a -> DeletionResult m a
--
delete3 (Right Empty) (Right a) (Right b) = Left $ node2 a b
delete3 (Right a) (Right Empty) (Right b) = Left $ node2 a b
delete3 (Right a) (Right b) (Right Empty) = Left $ node2 a b
--
delete3 (Right a) (Right b) (Right c) = Right $ node3 a b c
--
delete3 x y z = case concatMap deletionSubtrees [x, y, z] of
  [a, b, c, d, e] -> Right $ node2 (node2 a b) (node3 c d e)
  [a, b, c, d, e, f] -> Right $ node2 (node3 a b c) (node3 d e f)
  [a, b, c, d, e, f, g] -> Right $ node3 (node2 a b) (node2 c d) (node3 e f g)
  _ -> undefined

finishDeletion :: (Measured m a) => DeletionResult m a -> Tree m a
finishDeletion (Right tree) = tree
finishDeletion (Left tree) = tree
