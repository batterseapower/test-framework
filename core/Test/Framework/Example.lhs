== RUNNING ==

ghc -package test-framework -threaded Example.lhs -o Example
./Example --maximum-generated-tests=5000 +RTS -N2


== ATTRIBUTION ==

Tthe example properties come from the parallel QuickCheck driver (pqc),
see http://code.haskell.org/~dons/code/pqc/.  The BSD license is repeated
below, per the licensing conditions of pqc.

== LICENSING ==

Copyright Don Stewart 2006.

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Don Stewart nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

\begin{code}

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck

import Test.QuickCheck
import Test.HUnit

import Data.List


main = defaultMain tests

tests = [
        testGroup "Sorting Group 1" [
                testProperty "sort1" prop_sort1,
                testProperty "sort2" prop_sort2,
                testProperty "sort3" prop_sort3
            ],
        testGroup "Sorting Group 2" [
                testProperty "sort4" prop_sort4,
                testProperty "sort5" prop_sort5,
                testProperty "sort6" prop_sort6,
                testCase "sort7" test_sort7,
                testCase "sort8" test_sort8
            ]
    ]


prop_sort1 xs = sort xs == sortBy compare xs
  where types = (xs :: [Int])

prop_sort2 xs =
        (not (null xs)) ==>
        (head (sort xs) == minimum xs)
  where types = (xs :: [Int])

prop_sort3 xs = (not (null xs)) ==>
        last (sort xs) == maximum xs
  where types = (xs :: [Int])

prop_sort4 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (head (sort (xs ++ ys)) == min (minimum xs) (minimum ys))
  where types = (xs :: [Int], ys :: [Int])

prop_sort5 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (head (sort (xs ++ ys)) == max (maximum xs) (maximum ys))
  where types = (xs :: [Int], ys :: [Int])

prop_sort6 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (last (sort (xs ++ ys)) == max (maximum xs) (maximum ys))
  where types = (xs :: [Int], ys :: [Int])

test_sort7 = sort [8, 7, 2, 5, 4, 9, 6, 1, 0, 3] @?= [0..9]

test_sort8 = error "This test deliberately contains a user error"
\end{code}