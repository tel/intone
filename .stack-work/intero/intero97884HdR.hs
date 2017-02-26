{-# LANGUAGE TypeFamilies #-}

module Music.Intonation.Pitch where

import Control.Lens
import Music.Intonation.Internal.Vectors

newtype Pitch =
  Pitch { logHz :: Double }
  deriving (Eq, Ord, Show)

hz :: Iso' Pitch Double
hz = iso to fro where
  to = exp . logHz
  fro = Pitch . log

a440 :: Pitch
a440 = 440.0 ^. from hz

newtype Interval =
  Interval { logDiff :: Double }
  deriving (Eq, Ord, Show)

octave :: Interval
octave = Interval (log 2)

instance Additive Interval where
  -- Since Interval acts in the log domain, it just steals its additive group
  -- directly from Double
  zero = Interval (log 1)
  Interval a ^+^ Interval b = Interval (a + b)
  Interval a ^-^ Interval b = Interval (a - b)
  inverse (Interval a) = Interval (-a)

instance Affine Pitch where
  type Diff Pitch = Interval
  Pitch a .-. Pitch b = Interval (a - b)
  Pitch a .+^ Interval d = Pitch (a + d)
  Pitch a .-^ Interval d = Pitch (a - d)
