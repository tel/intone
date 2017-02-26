
module Music.Intonation.EqualTemperment.Notation where

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

{-

We cannonicalize the notes

1  - A
2  - A#
3  - B
4  - C
5  - C#
6  - D
7  - D#
8  - E
9  - F
10 - F#
11 - G
12 - G#

-}

newtype Note = Note Int
  deriving (Eq, Ord, Show)
