{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task1 where

-- * Measure

-- | Class describing values 'a' that can be measured as monoid 'm'
class (Monoid m) => Measured m a where
  -- | Returns corresponding measure 'm' for given value 'a'
  measure :: a -> m

-- Note: Intentionally marked as INCOHERENT to avoid clashes with more precise instances later
instance {-# INCOHERENT #-} (Monoid m) => Measured m m where
  measure = id

-- Note: Intentionally marked as INCOHERENT to avoid clashes with more precise instances later
instance {-# INCOHERENT #-} (Measured m a) => Measured m [a] where
  measure = foldMap measure

-- | Min
--
-- * Monoid:  @ Min S = {S, +inf, min} @
-- * Measure: @ measure x = Min x @
--
-- Usage example:
--
-- >>> measure "bar" :: Min Char
-- Min 'a'
-- >>> measure [True] :: Min Bool
-- Min True
-- >>> measure ([] :: [Int]) :: Min Int
-- PosInf
data Min a = PosInf | Min a
  deriving (Show, Eq)

instance (Ord a) => Semigroup (Min a) where
  PosInf <> r = r
  l <> PosInf = l
  (Min l) <> (Min r) = Min (min l r)

instance (Ord a) => Monoid (Min a) where
  mempty = PosInf

instance (Ord a) => Measured (Min a) a where
  measure = Min

-- | Max
--
-- * Monoid:  @ Max S = {S, -inf, max} @
-- * Measure: @ measure x = Max x @
--
-- Usage example:
--
-- >>> measure "bar" :: Max Char
-- Max 'r'
-- >>> measure [True] :: Max Bool
-- Max True
-- >>> measure ([] :: [Int]) :: Max Int
-- NegInf
data Max a = NegInf | Max a
  deriving (Show, Eq)

instance (Ord a) => Semigroup (Max a) where
  NegInf <> r = r
  l <> NegInf = l
  (Max l) <> (Max r) = Max (max l r)

instance (Ord a) => Monoid (Max a) where
  mempty = NegInf

instance (Ord a) => Measured (Max a) a where
  measure = Max

-- | MinMax
--
-- * Monoid:  @ MinMax S = {S, +inf, min} x {S, -inf, max} @
-- * Measure: @ measure x = MinMax (Min x, Max x) @
--
-- Usage example:
--
-- >>> measure "foo" :: MinMax Char
-- MinMax {getMinMax = (Min 'f',Max 'o')}
-- >>> measure [True] :: MinMax Bool
-- MinMax {getMinMax = (Min True,Max True)}
-- >>> measure ([] :: [Int]) :: MinMax Int
-- MinMax {getMinMax = (PosInf,NegInf)}
newtype MinMax a = MinMax {getMinMax :: (Min a, Max a)}
  deriving (Show, Eq)

instance (Ord a) => Semigroup (MinMax a) where
  (MinMax (lmin, lmax)) <> (MinMax (rmin, rmax)) = MinMax (lmin <> rmin, lmax <> rmax)

instance (Ord a) => Monoid (MinMax a) where
  mempty = MinMax (PosInf, NegInf)

instance (Ord a) => Measured (MinMax a) a where
  measure x = MinMax (Min x, Max x)

instance {-# INCOHERENT #-} (Ord a, Measured (MinMax a) t) => Measured (Min a) t where
  measure = fst . getMinMax . measure

instance {-# INCOHERENT #-} (Ord a, Measured (MinMax a) t) => Measured (Max a) t where
  measure = snd . getMinMax . measure

-- | Size
--
-- * Monoid:  @ Size S = {S, 0, (+)} @
-- * Measure: @ measure x = Size 1 @
--
-- Usage example:
--
-- >>> measure "bar" :: Size Char
-- Size {getSize = 3}
-- >>> measure [True] :: Size Bool
-- Size {getSize = 1}
-- >>> measure ([] :: [Int]) :: Size Int
-- Size {getSize = 0}
newtype Size a = Size {getSize :: Int}
  deriving (Show, Eq)

instance Semigroup (Size a) where
  (Size l) <> (Size r) = Size (l + r)

instance Monoid (Size a) where
  mempty = Size 0

instance Measured (Size a) a where
  measure = const (Size 1)
