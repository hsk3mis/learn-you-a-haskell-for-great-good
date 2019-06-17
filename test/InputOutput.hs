import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, (@=?))
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

main = defaultMain unitTestsInputOutput

unitTestsInputOutput = testGroup "Unit tests Input Output"
  [
    testCase "intersparse" $ assertEqual [] "M.O.N.K.E.Y" $ intersperse '.' "MONKEY"

  ]
