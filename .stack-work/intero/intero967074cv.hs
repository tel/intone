{-# LANGUAGE TypeFamilies #-}

module Music.Intonation.EqualTemperment where

import Control.Lens
import Music.Intonation.Internal.Vectors

data Letter
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  deriving (Eq, Ord, Show, Enum)

data Modifier
  = Sharp
  | Flat
  | Natural
  deriving (Eq, Show)

modifierSymbol :: Modifier -> String
modifierSymbol m = case m of
  Sharp -> "#"
  Flat -> "b"
  Natural -> ""

data ScalePosition =
  ScalePosition { letter :: Letter, modifier :: Modifier }
  deriving (Eq)

instance Show ScalePosition where
  show (ScalePosition letter modifier) =
    show letter ++ modifierSymbol modifier

-- | Listing of the @canonicalize@d @ScalePosition@s in their numbered order.
sharpPositions :: [ScalePosition]
sharpPositions =
  [ ScalePosition A Natural
  , ScalePosition A Sharp
  , ScalePosition B Natural
  , ScalePosition C Natural
  , ScalePosition C Sharp
  , ScalePosition D Natural
  , ScalePosition D Sharp
  , ScalePosition E Natural
  , ScalePosition F Natural
  , ScalePosition F Sharp
  , ScalePosition G Natural
  , ScalePosition G Sharp
  ]

-- | Normalizes a scale positions to the ones listed in @sharpPositions@.
-- Idempotent.
canonicalize :: ScalePosition -> ScalePosition
canonicalize (ScalePosition F Flat) = ScalePosition E Natural
canonicalize (ScalePosition C Flat) = ScalePosition B Natural
canonicalize (ScalePosition n Flat) = ScalePosition (pred n) Sharp
canonicalize sp = sp

-- | Two @ScalePosition@s with the same @canonicalize@d form are enharmonic.
enharmonic :: ScalePosition -> ScalePosition -> Bool
enharmonic a b = canonicalize a == canonicalize b

-- | @ScalePosition@s are isomorphic to integers mod 12.
scalePositionIndex :: Iso' ScalePosition Int
scalePositionIndex = iso to fro where
  to :: ScalePosition -> Int
  to pos = case canonicalize pos of
    ScalePosition A Natural -> 0
    ScalePosition A Sharp -> 1
    ScalePosition B Natural -> 2
    ScalePosition C Natural -> 3
    ScalePosition C Sharp -> 4
    ScalePosition D Natural -> 5
    ScalePosition D Sharp -> 6
    ScalePosition E Natural -> 7
    ScalePosition F Natural -> 8
    ScalePosition F Sharp -> 9
    ScalePosition G Natural -> 10
    ScalePosition G Sharp -> 11
  fro :: Int -> ScalePosition
  fro n =
    let
      modded = n `mod` 12
      ix = if (n >= 0) then n else 12 + n
    in
      sharpPositions !! ix

{-

We canonicalize the notes

0  - A
1  - A#
2  - B
3  - C
4  - C#
5  - D
6  - D#
7  - E
8  - F
9  - F#
10 - G
11 - G#

-}

-- NOTE: Private constructor so that we can guarantee that the index is always
-- on [0, 11]
data Note = Note Int Int
  deriving (Eq, Ord)

normal :: Int -> Int -> Note
normal oct ix =
  let (octShift, ix') = divMod ix 12 in Note (oct + octShift) ix'

instance Show Note where
  show (Note octave index) =
    show (index ^. from scalePositionIndex) ++ ":" ++ show octave

instance Enum Note where
  fromEnum (Note octave index) = 12 * octave + index
  toEnum n = let (octave, index) = divMod n 12 in Note octave index

c3 :: Note
c3 = Note 3 3

a4 :: Note
a4 = Note 4 0

octave :: Lens' Note Int
octave inj (Note o p) = fmap (\o' -> Note o' p) (inj o)

_spIndex :: Lens' Note Int
_spIndex inj (Note o p) = fmap (\o' -> Note o' p) (inj o)

scalePosition :: Lens' Note ScalePosition
scalePosition = _spIndex . from scalePositionIndex

-- | An @Interval@ is defined as a semitone count.
newtype Interval =
  Interval { semitones :: Int }
  deriving (Eq, Ord, Show)

instance Additive Interval where
  zero = Interval 0
  Interval a ^+^ Interval b = Interval (a + b)
  Interval a ^-^ Interval b = Interval (a - b)
  inverse (Interval a) = Interval (negate a)

instance Vector Interval where
  type Scalar Interval = Int
  n *^ Interval m = Interval (n * m)

instance Spanned Interval where
  type Basis Interval = B1
  based B1'1 = Interval 1
  measure (Interval a) B1'1 = a

instance Affine Note where
  type Diff Note = Interval
  n1 .-. n2 = Interval (fromEnum n1 - fromEnum n2)
  Note oct ix .+^ Interval sts = normal oct (ix + sts)
  Note oct ix .-^ Interval sts = normal oct (ix - sts)
