import Lib (double, half)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main = defaultMain unitTests

unitTests = testGroup "Unit tests"
  [simpleTest, doublingMakesNumbersBigger, halvingMakesNumbersSmaller]

simpleTest =
  testCase "Simplest test" $ assertEqual [] "abc" "abc"

doublingMakesNumbersBigger =
  testCase "Double of 4 is 8" $ assertEqual [] 8 (double 4)

halvingMakesNumbersSmaller =
  testCase "Half of 9 is 4" $ assertEqual [] 4.5 (half 9)