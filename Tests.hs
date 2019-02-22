import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import Common

-- Testing is derived from
-- https://stackoverflow.com/questions/20331209/haskell-unit-testing#20331512

normalizeTest :: Assertion
normalizeTest  = normalize (2.0, 0.0, 0.0) @?= (1.0, 0.0, 0.0)

main :: IO ()
main = defaultMainWithOpts
       [testCase "normalize" normalizeTest]
       mempty
