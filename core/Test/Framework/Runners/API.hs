-- | This module exports everything that you need to be able to create your own test runner.
module Test.Framework.Runners.API (
        module Test.Framework.Runners.Options,
        TestRunner(..), runTestTree
    ) where

import Test.Framework.Runners.Options
import Test.Framework.Runners.Core
