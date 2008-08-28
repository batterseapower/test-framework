module Test.Framework.Seed where

import Test.Framework.Utilities

import System.Random

import Data.Char


data Seed = FixedSeed Int
          | RandomSeed

instance Show Seed where
    show RandomSeed    = "random"
    show (FixedSeed n) = show n

instance Read Seed where
    readsPrec prec xs = if map toLower random_prefix == "random"
                        then [(RandomSeed, rest)]
                        else map (FixedSeed `onLeft`) (readsPrec prec xs)
      where (random_prefix, rest) = splitAt 6 xs

newSeededStdGen :: Seed -> IO StdGen
newSeededStdGen (FixedSeed seed) = return $ mkStdGen seed
newSeededStdGen RandomSeed = newStdGen