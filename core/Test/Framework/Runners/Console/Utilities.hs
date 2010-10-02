module Test.Framework.Runners.Console.Utilities (
        hideCursorDuring
    ) where

import System.Console.ANSI
import System.IO

import Control.Exception.Extensible


hideCursorDuring :: IO a -> IO a
hideCursorDuring action = bracket hideCursor (const (showCursor >> hFlush stdout)) (const action)
