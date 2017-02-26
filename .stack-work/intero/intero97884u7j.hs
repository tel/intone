{-# LANGUAGE TypeFamilies #-}

module Music.Intonation.Pitch where

import Control.Lens
import Music.Intonation.Internal.Vectors

newtype Pitch =
  Pitch { logHz :: Double }
  deriving (Eq, Ord, Show)

instance Affine Pitch where
  type Diff Pitch = Interval
  Pitch a .-. Pitch b = Interval (a - b)
  Pitch a .+^ Interval d = Pitch (a + d)
  Pitch a .-^ Interval d = Pitch (a - d)

hz :: Iso' Pitch Double
hz = iso to fro where
  to = exp . logHz
  fro = Pitch . log

a440 :: Pitch
a440 = 440.0 ^. from hz

newtype Interval =
  Interval { logDiff :: Double }
  deriving (Eq, Ord, Show)

factor :: Iso' Interval Double
factor = iso to fro where
  to = exp . logDiff
  fro = Interval . log

-- | In equal temperment interval between notes, especially out of tune notes,
-- are denoted in @cents@ or (log domain) percentage of a @semitone@.
cents :: Iso' Interval Double
cents = iso to fro where
  to (Interval ivl) = 100 * (ivl / logDiff semitone)
  fro cts = Interval (logDiff semitone * (cts / 100))

instance Additive Interval where
  -- Since Interval acts in the log domain, it just steals its additive group
  -- directly from Double
  zero = Interval (log 1)
  Interval a ^+^ Interval b = Interval (a + b)
  Interval a ^-^ Interval b = Interval (a - b)
  inverse (Interval a) = Interval (-a)

instance Vector Interval where
  type Scalar Interval = Double
  mult *^ Interval logHz = Interval (mult * logHz)

-- | @approxWith unit goal@ reports @(n, i)@ such that @a = b * n + i@ and the
-- absolute size of @i@ is minimized. This is not quite the same as a remainder
-- as @i@ may be negative.
approxWith :: Interval -> Interval -> (Int, Interval)
approxWith (Interval unit) (Interval goal) =
  if abs errorOver <= abs errorUnder
  then (overshoot, Interval errorOver)
  else (undershoot, Interval errorUnder)
  where
    exact = goal / unit
    overshoot = round exact
    undershoot = truncate exact
    errorOver = goal - fromIntegral overshoot * unit
    errorUnder = goal - fromIntegral undershoot * unit

-- Pythagorean intervals

octave :: Interval
octave = Interval (log 2)

just5 :: Interval
just5 = Interval (log 3 - log 2)

just3 :: Interval
just3 = Interval (log 5 - log 4)

-- | Up a just fifth and down a just third
just3m :: Interval
just3m = just5 ^-^ just3

justSeventh :: Interval
justSeventh = Interval (log 7)

-- | A (Pythagorean) @comma@ is a useful interval in just intonation. Starting
-- at C and applying 12 just fifths arrives at an enharmonic B# in the 8th
-- octave, the same C moved up 12 octaves is 1 @comma@ flatter than this
-- enharmonic B#.
comma :: Interval
comma = (12 *^ just5) ^-^ (7 *^ octave)

-- Equal intervals

semitone :: Interval
semitone = octave ^/ 12
