module Test.Framework.Runners.Console.ProgressBar (
        Progress(..), showProgressBar
    ) where


data Progress = Progress Int Int

showProgressBar :: Int -> Progress -> String
showProgressBar width (Progress count total) = "[" ++ reverse (take progress_chars ('>' : repeat '=')) ++ replicate space_chars ' ' ++ "]"
  where
    -- The available width takes account of the enclosing brackets
    available_width = width - 2
    characters_per_tick = fromIntegral available_width / fromIntegral total :: Double
    progress_chars = round (characters_per_tick * fromIntegral count)
    space_chars = available_width - progress_chars