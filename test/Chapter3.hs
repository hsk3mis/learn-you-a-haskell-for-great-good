import Lib (double, half)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, (@=?))
import Control.Exception (evaluate, catch)

main = defaultMain unitTestsChapter3

unitTestsChapter3 = testGroup "Unit tests Chapter3"
  [
    patternMatchingWorksTopDown1, patternMatchingWorksTopDown2, patternMatchingTheWhole,
    guardsTest1, guardsTest2, guardsPatternsTest1, guardsPatternsTest2,
    whereConstructWithPatterns, caseExpressionsTest1, caseExpressionsTest2
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
bmiTell' wight height = bmiTell bmi
  where bmi = wight / height ^ 2

bmiTell bmi
  | bmi <= skinny = "Underweight"
  | bmi <= normal = "Normal"
  | bmi <= fat = "Fat"
  | otherwise   = "Whale"
  where skinny = 18.5
        normal = 25.0
        fat = 30.0
--  where (skinny, normal, fat) = (18.5, 25.0, 30.0)
-- all names in where must be aligned in the same column!!

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

-- WHERE BINDINGS
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

whereConstructWithPatterns =
  testCase "Where bindings can use pattern matching" $ assertEqual [] "J. D." (initials "John" "Do")

-- LET BINDINGS (multiple bindings in same column or separated with semicolon ";")
arrayOfTripletsOfSquaresTest = [let square x = x * x in (square 5, square 3, square 2)]

calcBmisWithLetInsideListComprehension xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]


-- CASE EXPRESSIONS (pattern matching on parameters of function definition is JUST a syntactic sugar for case expression)
head' [] = error "No head"
head' (x:_) = x

head'' xs = case xs of [] -> error "No head"
                       (x:_) -> x

describeList xs = "The list is " ++ what xs ++ "."
  where what [] = "empty"
        what [x] = "a singleton list"
        what xs = "a longer list"

caseExpressionsTest1 =
  testCase "Case Expressions test 1" $ assertEqual [] "The list is empty." (describeList [])
caseExpressionsTest2 =
  testCase "Case Expressions test 2" $ assertEqual [] "The list is a singleton list." (describeList [123])













