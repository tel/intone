{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Music.Intonation.Internal.Vectors where

class Additive v where

  infixl 6 ^+^
  infixl 6 ^-^

  zero :: v
  -- | Group sum operation
  (^+^) :: v -> v -> v

  -- | Group difference operation
  (^-^) :: v -> v -> v
  a ^-^ b = a ^+^ inverse b

  -- | Inverse element
  inverse :: v -> v
  inverse e = zero ^-^ e

class Additive (Diff p) => Affine p where
  type Diff p

  infixl 6 .+^
  infixl 6 .-.
  infixl 6 .-^

  -- | Difference between @Affine@ points
  (.-.) :: p -> p -> Diff p

  -- | Motion of an @Affine@ point
  (.+^) :: p -> Diff p -> p

  (.-^) :: p -> Diff p -> p
  p .-^ v = p .+^ inverse v

-- | Vectors can forget their origins and be treated as @Point@s
newtype Point v =
  Point v
  deriving (Eq, Ord, Show, Additive)

instance Additive f => Affine (Point f) where
  type Diff (Point f) = f
  Point x .-. Point y = x ^-^ y
  Point x .+^ v = Point (x ^+^ v)
  Point x .-^ v = Point (x ^-^ v)

infixl 7 ^*, *^, ^/

class Additive v => Vector v where
  type Scalar v

  -- | Scale a vector
  (*^) :: Scalar v -> v -> v

negated :: (Num (Scalar v), Vector v) => v -> v
negated v = v ^* (-1)

(^*) :: Vector v => v -> Scalar v -> v
v ^* s = s *^ v

(^/) :: (Fractional (Scalar v), Vector v) => v -> Scalar v -> v
v ^/ s = v ^* (recip s)

linearCombination :: (Foldable f, Vector v) => f (v, Scalar v) -> v
linearCombination = foldr smash zero where
  smash (comp, s) v = v ^+^ (comp ^* s)

class Vector v => Spanned v where
  type Basis v

  based :: Basis v -> v
  measure :: v -> Basis v -> Scalar v

-- | An @Enumerable@ type has a (given) means to visit every value.
class Enumerable a where
  enumeration :: [a]

decompose :: (Enumerable (Basis v), Spanned v) => v -> [(Basis v, Scalar v)]
decompose v = fmap measuring enumeration where
  measuring basis = (basis, measure v basis)

recompose :: (Enumerable (Basis v), Spanned v) => (Basis v -> Scalar v) -> v
recompose weight = foldr smash zero enumeration where
  smash b v = v ^+^ (based b ^* weight b)

recomposeFixed :: Spanned v => [(Basis v, Scalar v)] -> v
recomposeFixed pairs = foldr smash zero pairs where
  smash (b, s) v = v ^+^ (based b ^* s)

-- | Anonymous 0-dimensional basis
data B0

deriving instance Eq B0
deriving instance Ord B0
deriving instance Show B0

instance Enumerable B0 where
  enumeration = []

-- | Anonymous 1-dimensional basis
data B1 = B1'1 deriving (Eq, Ord, Show, Enum)

instance Enumerable B1 where
  enumeration = [B1'1]

-- | Anonymous 2-dimensional basis
data B2 = B2'1 | B2'2 deriving (Eq, Ord, Show, Enum)

instance Enumerable B2 where
  enumeration = [B2'1, B2'2]

-- | Anonymous 3-dimensional basis
data B3 = B3'1 | B3'2 | B3'3 deriving (Eq, Ord, Show, Enum)

instance Enumerable B3 where
  enumeration = [B3'1, B3'2, B3'3]
