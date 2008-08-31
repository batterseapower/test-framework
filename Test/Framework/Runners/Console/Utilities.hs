module Test.Framework.Runners.Console.Utilities where


eraseStr :: Int -> IO ()
eraseStr num = putStr (replicate num '\b')

putStrIndented :: Int -> String -> IO ()
putStrIndented how_much = putStr . indent how_much

putStrLnIndented :: Int -> String -> IO ()
putStrLnIndented how_much = putStrLn . indent how_much

indent :: Int -> String -> String
indent how_much what = (replicate how_much ' ') ++ what