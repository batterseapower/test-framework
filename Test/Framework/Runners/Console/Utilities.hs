module Test.Framework.Runners.Console.Utilities (
        hideCursorIn
    ) where

import System.Console.ANSI

import Control.Exception

import Prelude hiding (catch)


hideCursorIn :: IO () -> IO ()
hideCursorIn action = do
    hideCursor
    catch (action >> showCursor) (\exception -> showCursor >> throwIO exception)
