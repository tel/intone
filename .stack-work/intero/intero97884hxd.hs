{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib where

import GHC.Real

data NoteName
  = NoteA
  | NoteASharp
  | NoteB
  | NoteC
  | NoteCSharp
  | NoteD
  | NoteDSharp
  | NoteE
  | NoteF
  | NoteFSharp
  | NoteG
  | NoteGSharp
  deriving (Eq, Enum)

instance Show NoteName where
  show n = case n of
    NoteA -> "A"
    NoteASharp -> "A#"
    NoteB -> "B"
    NoteC -> "C"
    NoteCSharp -> "C#"
    NoteD -> "D"
    NoteDSharp -> "D#"
    NoteE -> "E"
    NoteF -> "F"
    NoteFSharp -> "F#"
    NoteG -> "G"
    NoteGSharp -> "G#"

data Et = Et { etOctave :: Int, note :: NoteName }

instance Show Et where
  show Et { etOctave, note } = show note ++ "-" ++ show etOctave

etUp :: Et -> Et
etUp Et { etOctave, note } =
  case note of
    NoteA -> Et { etOctave = etOctave, note = NoteASharp }
    NoteASharp -> Et { etOctave = etOctave, note = NoteB }
    NoteB -> Et { etOctave = etOctave, note = NoteC }
    NoteC -> Et { etOctave = etOctave, note = NoteCSharp }
    NoteCSharp -> Et { etOctave = etOctave, note = NoteD }
    NoteD -> Et { etOctave = etOctave, note = NoteDSharp }
    NoteDSharp -> Et { etOctave = etOctave, note = NoteE }
    NoteE -> Et { etOctave = etOctave, note = NoteF }
    NoteF -> Et { etOctave = etOctave, note = NoteFSharp }
    NoteFSharp -> Et { etOctave = etOctave, note = NoteG }
    NoteG -> Et { etOctave = etOctave, note = NoteGSharp }
    NoteGSharp -> Et { etOctave = etOctave + 1, note = NoteA }

etDown :: Et -> Et
etDown Et { etOctave, note } =
  case note of
    NoteA -> Et { etOctave = etOctave - 1, note = NoteGSharp }
    NoteASharp -> Et { etOctave = etOctave, note = NoteA }
    NoteB -> Et { etOctave = etOctave, note = NoteASharp }
    NoteC -> Et { etOctave = etOctave, note = NoteB }
    NoteCSharp -> Et { etOctave = etOctave, note = NoteC }
    NoteD -> Et { etOctave = etOctave, note = NoteCSharp }
    NoteDSharp -> Et { etOctave = etOctave, note = NoteD }
    NoteE -> Et { etOctave = etOctave, note = NoteDSharp }
    NoteF -> Et { etOctave = etOctave, note = NoteE }
    NoteFSharp -> Et { etOctave = etOctave, note = NoteF }
    NoteG -> Et { etOctave = etOctave, note = NoteFSharp }
    NoteGSharp -> Et { etOctave = etOctave, note = NoteG }

chromaticUp :: Et -> [Et]
chromaticUp = iterate etUp

chromaticDown :: Et -> [Et]
chromaticDown = iterate etDown

a4 :: Et
a4 = Et 4 NoteA

c3 :: Et
c3 = Et 3 NoteC

semitoneDiff :: Et -> Et -> Int
semitoneDiff et1 et2 = 12 * octaveShift + noteShift where
  octaveShift = etOctave et1 - etOctave et2
  noteShift = fromEnum (note et1) - fromEnum (note et2)

newtype Pitch =
  Pitch { hz :: Double }
  deriving (Eq, Ord, Show, Num, Floating, Fractional, Real, RealFrac)

semitoneDelta :: Pitch
semitoneDelta = 2.0 ** (1.0/12.0)

etPitch :: Et -> Pitch
etPitch (Et { etOctave = 4, note = NoteA}) = 440.0
etPitch et = etPitch a4 * (semitoneDelta ** semitones) where
  semitones = fromIntegral $ semitoneDiff et a4

newtype Ji =
  Ji { steps :: [Int] }
  deriving (Eq, Show)

data Dir = X2 | X3 | X5 | X7
  deriving (Eq, Ord, Show, Enum)

step :: Dir -> Ji
step X2 = Ji [1]
step X3 = Ji [0, 1]
step X5 = Ji [0, 0, 1]
step X7 = Ji [0, 0, 0, 1]

scale :: Int -> Ji -> Ji
scale n (Ji coords) = Ji (map (* n) coords)

zipWithFilling :: a -> b -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithFilling za zb phi = go where
  go [] [] = []
  go (a:as) [] = phi a zb : go as []
  go [] (b:bs) = phi za b : go [] bs
  go (a:as) (b:bs) = phi a b : go as bs

add :: Ji -> Ji -> Ji
add (Ji c1) (Ji c2) = Ji (zipWithFilling 0 0 (+) c1 c2)

root :: Ji
root = Ji []

move :: (Dir, Int) -> (Ji -> Ji)
move (d, n) = add (scale n (step d))

move1 :: Dir -> Ji -> Ji
move1 d = move (d, 1)

primes :: [Int]
primes = [2, 3, 5, 7, 11, 13]

jiPitch :: Ji -> (Pitch -> Pitch)
jiPitch (Ji coords) root = root * ratio where
  -- NOTE: consider doing this in log domain
  ratio = product jumps
  jumps = zipWith raise primes coords
  raise prime power = fromIntegral prime ** fromIntegral power

data JiRatio = JiRatio { octaveShift :: Int, ratio :: Ratio Int }

instance Show JiRatio where
  show JiRatio { octaveShift, ratio } =
    show ratio ++ " (" ++ show octaveShift ++ ")"

rationalPower :: Int -> Ratio Int -> Ratio Int
rationalPower n (numer :% denom)
  | n == 0 = 1
  | n > 0 = (numer ^ n) :% (denom ^ n)
  | n < 0 = (denom ^ negate n) :% (numer ^ negate n) 

ratioRepr :: Ji -> JiRatio
ratioRepr (Ji coords) = case coords of
  [] -> JiRatio 0 1
  (oct:rest) ->
    let
      jumps = zipWith rationalPower rest (map fromIntegral $ tail primes)
      numer :% denom = product jumps
      (div, mod) = divMod numer denom
    in
      case div of
        -- When 1, we're right on the money. No octave shift.
        1 -> JiRatio oct (reduce numer denom)

        -- When 0, this means that we have a ratio less than 1/1
        -- Go up ceiling (log2 denom) octaves
        0 ->
          let
            octavesUp :: Int
            octavesUp =
              ceiling (log (fromIntegral denom) / log 2)
          in
            JiRatio (oct - octavesUp) (reduce (2 ^ octavesUp * numer) denom)

        -- When >= 2, we've got a ratio above 2/1
        -- Go down floor (log2 numer) octaves
        _ ->
          let
            octavesDown :: Int
            octavesDown =
              floor (log (fromIntegral numer) / log 2)
          in
            JiRatio (oct + octavesDown) (reduce numer (2 ^ octavesDown * denom))

