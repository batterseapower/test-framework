module Test.Framework.Utilities where

import Data.Maybe
import Data.Monoid


data K a = K { unK :: a }


putStrIndented :: Int -> String -> IO ()
putStrIndented how_much = putStr . indent how_much

putStrLnIndented :: Int -> String -> IO ()
putStrLnIndented how_much = putStrLn . indent how_much

indent :: Int -> String -> String
indent how_much what = (replicate how_much ' ') ++ what

mappendBy :: Monoid b => (a -> b) -> a -> a -> b
mappendBy f left right = (f left) `mappend` (f right)

orElse :: Maybe a -> a -> a
orElse = flip fromMaybe

onLeft :: (a -> c) -> (a, b) -> (c, b)
onLeft f (x, y) = (f x, y)

onRight :: (b -> c) -> (a, b) -> (a, c)
onRight f (x, y) = (x, f y)