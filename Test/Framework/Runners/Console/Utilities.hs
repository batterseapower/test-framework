module Test.Framework.Runners.Console.Utilities (
        hideCursorIn
    ) where

import System.Console.ANSI

import Control.Exception

import Prelude hiding (catch)


hideCursorIn :: IO a -> IO a
hideCursorIn action = bracket hideCursor (const showCursor) (const action)
