{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Framework.Tests.Runners.XMLTests (
    test, property
  ) where

import Test.Framework.Runners.Core ( RunTest(..), FinishedTest )
import Test.Framework.Runners.XML.JUnitWriter ( RunDescription(..), morphFlatTestCase, serialize )

import Test.HUnit.Base               ( Test(..), (@?=) )
import Test.QuickCheck               ( Arbitrary, sized, Gen, oneof, listOf, arbitrary )
import Test.QuickCheck.Property as P ( Property )

import Control.Monad

import Data.ByteString.Char8 as BS ( pack )
import Data.Maybe

import qualified Text.XML.Light as XML         ( findAttr, unqual )
import qualified Text.XML.LibXML.Parser as XML ( parseMemory_ )
import qualified Text.XML.LibXML.Types as XML  ( Document )


-- #ifdef MIN_VERSION_QuickCheck(2, 4, 0)
import Test.QuickCheck.Property as P (morallyDubiousIOProperty)
-- #else
-- import qualified Test.QuickCheck.Property as P (succeeded, failed, liftIOResult)

-- morallyDubiousIOProperty :: IO Bool -> Property
-- morallyDubiousIOProperty it = P.liftIOResult $ fmap (\err -> if err then P.failed else P.succeeded) it
-- #endif

-- | `Arbitrary` instance for `TestResult` generation.
instance Arbitrary FinishedTest where
  arbitrary = sized testResult

-- | Size-constrained generator for `TestResult`
testResult :: Int -> Gen FinishedTest
testResult n | n <= 0    = arbitraryTR
             | otherwise = oneof [ liftM2 RunTestGroup arbitraryXmlStr (listOf subResult),
                                  subResult]
               where arbitraryTR = liftM3 RunTest arbitraryXmlStr arbitraryXmlStr
                                   (liftM2 (,) arbitraryXmlStr arbitrary)
                     -- | drastically cut the size at each level.
                     -- round .. -1 is a hack.  It works a bit better
                     -- (is more extreme) than floor and we're really
                     -- just trying to bound the size so that the
                     -- tests finish quickly.  To see how @floor /=
                     -- (-1) + round@ consider the inputs: 0.5, 1.5,
                     -- and 2.5.
                     subResult :: Gen FinishedTest
                     subResult = let reduce x = (round (logBase 32 (fromIntegral x) :: Double)) - 1
                                 in testResult $ reduce n

-- | `RunDescription` generator.  All string records are restricted to valid xml characters.
instance Arbitrary RunDescription where
  arbitrary = do
              return RunDescription
              `ap` arbitrary            -- errors
              `ap` arbitrary            -- failed count
              `ap` arbitrary            -- skipped
              `ap` arbitraryMaybeXmlStr -- hostname
              `ap` arbitraryXmlStr      -- suiteName
              `ap` arbitrary            -- testCount
              `ap` arbitrary            -- time
              `ap` arbitraryMaybeXmlStr -- timeStamp
              `ap` arbitraryMaybeXmlStr -- runId
              `ap` arbitraryMaybeXmlStr -- package
              `ap` arbitrary            -- tests

-- | Generator for strings that only contain valid XML codepoints, and
-- are wrapped in Maybe.  If/when empty strings are generated, they
-- have a 50% chance of being `Nothing`, so this generator should be biased
-- to create `Just` `String`s over `Nothing`
arbitraryMaybeXmlStr :: Gen (Maybe String)
arbitraryMaybeXmlStr = do
  str <- arbitraryXmlStr
  if null str  -- if we have an empty string, we have a chance of generating @Nothing@
    then oneof [return (Just str), return Nothing]
    else return (Just str)

-- | String generator that does not include invalid XML characters.  The
-- set of invalid characters is specified here:
-- http://www.w3.org/TR/2000/REC-xml-20001006#NT-Char
arbitraryXmlStr :: Gen String
arbitraryXmlStr = listOf arbitraryXmlChar
  where
    arbitraryXmlChar :: Gen Char
    arbitraryXmlChar = do c <- arbitrary
                          if validXmlChar (fromEnum c)
                            then return c
                            else arbitraryXmlChar
    validXmlChar c = c == 0x9 || c == 0xA || c == 0xD
                         || (c >= 0x20 && c <= 0xD7FF)
                         || (c >= 0xE000 && c <= 0xFFFD)
                         || (c >= 0x10000 && c <= 0x10FFFF)

-- | Generate random `RunDescriptions`, serialize to (flat) XML strings, then check that they are XML
-- TODO: check them against the JUnit schema
property :: RunDescription -> P.Property
property = morallyDubiousIOProperty . fmap isJust . parseSerialize

parseSerialize :: RunDescription -> IO (Maybe XML.Document)
parseSerialize = XML.parseMemory_ . BS.pack . serialize False

-- | Verify that the group names are properly pre-pended to sub-tests.
test :: Test
test = TestLabel "Check the composition of group names" $ TestCase $
       XML.findAttr (XML.unqual "classname") x @?= Just "top.g1"
  where x = head $ morphFlatTestCase [] $ RunTestGroup "top" [RunTestGroup "g1" [RunTest "t1" "" ("", True)]]
