
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
