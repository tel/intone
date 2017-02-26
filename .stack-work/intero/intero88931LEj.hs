module Lib
    ( someFunc
    ) where

newtype Pitch = Pitch { hz :: Double }

a4Pitch :: Pitch
a4Pitch = Pitch 440

equalTempermentDelta :: Pitch

someFunc :: IO ()
someFunc = putStrLn "someFunc"
