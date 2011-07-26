module Test.Framework.Runners.XML.JUnitWriter (
        RunDescription(..),
        serialize, toXml,
#ifdef TEST
        morphTestCase
#endif
    ) where

import Test.Framework.Runners.Core (RunTest(..), FinishedTest)

import Data.Maybe ( fromMaybe )
import Text.XML.Light ( ppTopElement, unqual, unode
                      , Attr(..), Element(..) )


-- | An overall description of the test suite run.  This is currently
-- styled after the JUnit xml.  It contains records that are not yet
-- used, however, it provides a sensible structure to populate as we
-- are able, and the serialiazation code behaves as though these are
-- filled.
data RunDescription = RunDescription {
    errors :: Int -- ^ The number of tests that triggered error
                  -- conditions (unanticipated failures)
  , failedCount :: Int        -- ^ Count of tests that invalidated stated assertions.
  , skipped :: Maybe Int      -- ^ Count of tests that were provided but not run.
  , hostname :: Maybe String  -- ^ The hostname that ran the test suite.
  , suiteName :: String       -- ^ The name of the test suite.
  , testCount :: Int          -- ^ The total number of tests provided.
  , time :: Double            -- ^ The total execution time for the test suite.
  , timeStamp :: Maybe String -- ^ The time stamp that identifies when this run happened.
  , runId :: Maybe String     -- ^ Included for completness w/ junit.
  , package :: Maybe String   -- ^ holdover from Junit spec. Could be
                              -- used to specify the module under test.
  , tests :: [FinishedTest]   -- ^ detailed description and results for each test run.
  } deriving (Show)


-- | Serializes a `RunDescription` value to a `String`.
serialize :: RunDescription -> String
serialize = ppTopElement . toXml

-- | Maps a `RunDescription` value to an XML Element
toXml :: RunDescription -> Element
toXml runDesc = unode "testsuite" (attrs, map morphTestCase $ tests runDesc)
  where
    -- | Top-level attributes for the first @testsuite@ tag.
    attrs :: [Attr]
    attrs = map (\(x,f)->Attr (unqual x) (f runDesc)) fields
    fields = [ ("errors",    show . errors)
             , ("failures",  show . failedCount)
             , ("skipped",   fromMaybe "" . fmap show . skipped)
             , ("hostname",  fromMaybe "" . hostname)
             , ("name",      id . suiteName)
             , ("tests",     show . testCount)
             , ("time",      show . time)
             , ("timeStamp", fromMaybe "" . timeStamp)
             , ("id",        fromMaybe "" . runId)
             , ("package",   fromMaybe "" . package)
             ]

-- | Generates XML elements for an individual test case or test group.
morphTestCase :: FinishedTest -> Element
morphTestCase (RunTestGroup gname testList) =
  unode "testsuite" (attrs, map morphTestCase testList)
  where attrs = [ Attr (unqual "name") gname ]

morphTestCase (RunTest tName _ (tout, pass)) = case pass of
  True  -> unode "testcase" caseAttrs
  False -> unode "testcase" (caseAttrs, unode "failure" (failAttrs, tout))
  where caseAttrs = [ Attr (unqual "name") tName
                    , Attr (unqual "classname") ""
                    , Attr (unqual "time") ""
                    ]
        failAttrs = [ Attr (unqual "message") ""
                    , Attr (unqual "type") ""
                    ]
