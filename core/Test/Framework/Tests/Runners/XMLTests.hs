{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Framework.Tests.Runners.XMLTests where

import Test.Framework.Runners.Core ( RunTest(..), FinishedTest )
import Test.Framework.Runners.XML.JUnitWriter ( RunDescription(..), morphTestCase, serialize )

import Test.HUnit.Base               ( Test(..), Assertion, (@?=) )
import Test.QuickCheck               ( Arbitrary, sized, Gen, oneof, listOf, arbitrary )
import Test.QuickCheck.Property as P (Property, Result(..), succeeded, failed,
                                      result, liftIOResult)

import Control.Monad

import Data.ByteString.Char8 as C8 ( pack )
import Data.Maybe

import Text.XML.Light              ( findAttr, unqual, findElements )
import Text.XML.LibXML.Parser      ( parseMemory_ )

-- Properties:

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

-- | Generate random `RunDescriptions`, serialize to XML strings, then
-- compare against the junitreport schema.
prop_validXml :: RunDescription -> P.Property
prop_validXml runDescr =  P.liftIOResult $ simpleValidate $ serialize runDescr
  where
    simpleValidate :: String -> IO P.Result
    simpleValidate xml = do
        err <- fmap isNothing $ parseMemory_ $ C8.pack xml
        return $ if err then P.failed else P.succeeded


{-
   HUnit tests:
-}



tests :: [Test]
tests = [ ]

{- This test no longer applies
tests = [ TestLabel "Check the composition of group names"
          (TestCase test_gNameCase1)
        ]

-- | Verify that the group names are properly pre-pended to sub-tests.
test_gNameCase1 :: Assertion
test_gNameCase1 = let x = morphTestCase tGroup2
                  in
                   findAttr (unqual "classname") x @?= Just "top.g1"
                     where
                       tGroup1 = RunTestGroup "g1" [RunTest "t1" "" ("", True)]
                       tGroup2 = RunTestGroup "top" [tGroup1]
-}

