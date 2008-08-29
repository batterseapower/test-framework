module Test.Framework.Utilities where

import Data.Maybe
import Data.Monoid


data K a = K { unK :: a }


eraseStr :: Int -> IO ()
eraseStr num = putStr (replicate num '\b')

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

-- | Like 'unlines', but does not append a trailing newline if there
-- is at least one line.  For example:
--
-- > unlinesConcise ["A", "B"] == "A\nB"
-- > unlinesConcise [] == ""
--
-- Whereas:
--
-- > unlines ["A", "B"] == "A\nB\n"
-- > unlines [] == ""
--
-- This is closer to the behaviour of 'unwords', which does not append
-- a trailing space.
unlinesConcise :: [String] -> String
unlinesConcise []     = []
unlinesConcise [l]    = l
unlinesConcise (l:ls) = l ++ '\n' : unlinesConcise ls
