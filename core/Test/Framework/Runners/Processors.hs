module Test.Framework.Runners.Processors (
        processorCount
    ) where

#ifdef COMPILER_GHC

import GHC.Conc ( numCapabilities )

processorCount :: Int
processorCount = numCapabilities

#else

processorCount :: Int
processorCount = 1

#endif