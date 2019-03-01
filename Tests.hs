import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import Common

-- Testing is derived from
-- https://stackoverflow.com/questions/20331209/haskell-unit-testing#20331512

normalizeTest1 :: Assertion
normalizeTest1  = normalize (2.0, 0.0, 0.0) @?= (1.0, 0.0, 0.0)

normalizeTest2 :: Assertion
normalizeTest2  = normalize (0.0, 1.0, 0.0) @?= (0.0, 1.0, 0.0)

normalizetests = [testCase "normalize1" normalizeTest1,
                  testCase "normalize2" normalizeTest2]

dotTest1 :: Assertion
dotTest1  = dotProduct (1.0, 4.0, 0.0) (1.0, 2.0, 1.0) @?= 9.0

dotTest2 :: Assertion
dotTest2  = dotProduct (0.0, 1.0, 0.0) (1.0, 0.0, 1.0) @?= 0.0

dottests = [testCase "dotProduct1" dotTest1,
            testCase "dotProduct2" dotTest2]

addVecTest1 :: Assertion
addVecTest1  = addVectors (1.0, 4.0, 0.0) (1.0, 2.0, 1.0) @?= (2.0, 6.0, 1.0)

addVecTest2 :: Assertion
addVecTest2  = addVectors (0.0, 1.0, 0.0) (1.0, 0.0, 1.0) @?= (1.0, 1.0, 1.0)

subVecTest1 :: Assertion
subVecTest1  = subVectors (1.0, 4.0, 0.0) (1.0, 2.0, 1.0) @?= (0.0, 2.0, -1.0)

subVecTest2 :: Assertion
subVecTest2  = subVectors (0.0, 1.0, 0.0) (1.0, 0.0, 1.0) @?= (-1.0, 1.0, -1.0)

vecmathtests = [testCase "addVec1" addVecTest1,
                testCase "addVec2" addVecTest2,
                testCase "subVec1" subVecTest1,
                testCase "subVec2" subVecTest2]

magnitudeTest1 :: Assertion
magnitudeTest1  = magnitude (1, 2, 3) @?= sqrt(1.0 * 1.0 + 2.0 * 2.0 + 3.0 * 3.0)

magnitudeTest2 :: Assertion
magnitudeTest2  = magnitude (4, 8, 2) @?= sqrt(4.0 * 4.0 + 8.0 * 8.0 + 2.0 * 2.0)

magnitudetests = [testCase "magnitude1" magnitudeTest1,
                  testCase "magnitude2" magnitudeTest2]

main :: IO ()
main = defaultMainWithOpts
       (normalizetests ++ dottests ++ vecmathtests ++ magnitudetests)
       mempty
