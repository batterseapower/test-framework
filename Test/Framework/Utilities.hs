module Test.Framework.Utilities where

import Data.Maybe
import Data.Monoid


newtype K a = K { unK :: a }


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

mapAccumLM :: Monad m => (acc -> x -> m (acc, y)) -> acc -> [x] -> m (acc, [y])
mapAccumLM _ acc [] = return (acc, [])
mapAccumLM f acc (x:xs) = do
    (acc', y) <- f acc x
    (acc'', ys) <- mapAccumLM f acc' xs
    return (acc'', y:ys)

padRight :: Int -> String -> String
padRight desired_length s = s ++ replicate (desired_length - length s) ' '