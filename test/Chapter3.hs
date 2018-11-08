import Lib (double, half)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, (@=?))
import Control.Exception (evaluate, catch)

main = defaultMain unitTestsChapter3

unitTestsChapter3 = testGroup "Unit tests Chapter3"
  [
    patternMatchingWorksTopDown1, patternMatchingWorksTopDown2, patternMatchingTheWhole,
    guardsTest1, guardsTest2, guardsPatternsTest1, guardsPatternsTest2
  ]

-- PATTERN MATCHING
matchtopDown 7 = "Lucky number"
matchtopDown x = "No luck"
matchtopDown 13 = "Impossible number"

patternMatchingWorksTopDown1 =
  testCase "Pattern matching: works top down" $ assertEqual [] "Lucky number" (matchtopDown 7)

patternMatchingWorksTopDown2 =
  testCase "Pattern matching: don't work for the overriding pattern" $ assertEqual [] "No luck" (matchtopDown 13)


capital "" = error "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

patternMatchingTheWhole =
  testCase "Pattern matching: the whole" $ assertEqual [] "The first letter of Dracula is D" (capital "Dracula")


-- GUARDS
bmiTell bmi
  | bmi <= 18.5 = "Underweight"
  | bmi <= 25.0 = "Normal"
  | bmi <= 30.0 = "Fat"
  | otherwise   = "Whale"

guardsTest1 =
  testCase "Guards: test 1" $ assertEqual [] "Normal" (bmiTell 20.0)

guardsTest2 =
  testCase "Guards: test 2" $ assertEqual [] "Whale" (bmiTell 31.0)

guardsPatternsIntegration x
  | x == 1 = "guarded: 1"
  | x == 2 = "guarded: 2"
guardsPatternsIntegration 3 = "pattern matched: 3"

guardsPatternsTest1 =
  testCase "Guards: with patterns 1" $ assertEqual [] "guarded: 1" (guardsPatternsIntegration 1)

guardsPatternsTest2 =
  testCase "Guards: with patterns 2" $ assertEqual [] "pattern matched: 3" (guardsPatternsIntegration 3)






