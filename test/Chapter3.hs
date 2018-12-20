import Lib (double, half)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, (@=?))
import Control.Exception (evaluate, catch)
import Data.List
import Test.QuickCheck

main = defaultMain unitTestsChapter3

unitTestsChapter3 = testGroup "Unit tests Chapter3"
  [
    patternMatchingWorksTopDown1, patternMatchingWorksTopDown2, patternMatchingTheWhole,
    guardsTest1, guardsTest2, guardsPatternsTest1, guardsPatternsTest2,
    whereConstructWithPatterns, caseExpressionsTest1, caseExpressionsTest2,
    shouldIdentifyPermutations, shouldIdentifyPermutations2, shouldIdentifyPermutations3, shouldIdentifyNonPermutations, shouldIdentifyNonPermutations2,
    sortedShouldIdentifyEmpty, sortedShouldIdentifySingleElement, sortedShouldIdentifySorted, sortedShouldNotIdentifyUnSorted,
    quickCheckPropSorted
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



-- EXCERCISES ABOUT LISTS
-- or :: [Bool] -> Bool, returns True if any element of its argument list is True.
-- and :: [Bool] -> Bool, returns True if every element of its argument list is True.
-- nub :: Eq a => [a] -> [a], which removes duplicate elements from a list.

-- Permutations: checking if two lists are permutations
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] ys = False
isPermutation (x:xs) ys = (x `elem` ys) && (isPermutation xs (ys `listWithoutOnce` x))

[]     `listWithoutOnce` x = []
(y:ys) `listWithoutOnce` x | y == x    = ys
                           | otherwise = y : (ys `listWithoutOnce` x)

shouldIdentifyPermutations =
  testCase "Permutations: true for itself" $ assertEqual [] True (isPermutation [1,2,3] [1,2,3])

shouldIdentifyPermutations2 =
  testCase "Permutations: true" $ assertEqual [] True (isPermutation [1,2,1] [2,1,1])

shouldIdentifyPermutations3 =
  testCase "Permutations: true for two empty lists" $ assertEqual [] True (isPermutation [] ([]::[Integer]))

shouldIdentifyNonPermutations =
  testCase "Permutations: false" $ assertEqual [] False (isPermutation [1,2,1] [1,2,2])

shouldIdentifyNonPermutations2 =
  testCase "Permutations: false for empty list" $ assertEqual [] False (isPermutation [] [1,2,2])

-- Sorting
-- check if sorted
sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:tail@(y:ys)) = (x <= y) && (sorted tail)

sortedShouldIdentifyEmpty =
  testCase "Sorting: sorted should identify empty list" $ assertEqual [] True (sorted ([]::[Int]))

sortedShouldIdentifySingleElement =
  testCase "Sorting: sorted should identify single element list" $ assertEqual [] True (sorted [1])

sortedShouldIdentifySorted =
  testCase "Sorting: sorted should identify sorted list" $ assertEqual [] True (sorted [1..10])

sortedShouldNotIdentifyUnSorted =
  testCase "Sorting: sorted should not identify unsorted list" $ assertEqual [] False (sorted [10,9..1])


-- insert element into sorted list
insertToSorted :: Ord a => a -> [a] -> [a]
x `insertToSorted` [] = [x]
x `insertToSorted` (tail@(y:ys)) | x <= y    = x : tail
                            | otherwise = y : (x `insertToSorted` ys)

-- insertion sort
insertionSort :: Ord a => [a] -> [a]
insertionSort xs = insertionSort' xs []

insertionSort' [] ys = ys
insertionSort' (x:xs) ys = insertionSort' xs (x `insertToSorted` ys)

-- Pascal's Triangle (return n'th row of Pascal's Triangle)
--       1
--     1   1
--   1   2   1
-- 1   3   3   1
pascal :: Int -> [Int]

-- n'th row has n elements and starts and ends with 1
pascal 1 = [1]
pascal n = [1] ++ sumsOfPairs (pascal (n-1)) ++ [1]
  where
    sumsOfPairs (x:y:ys) = (x+y) : sumsOfPairs (y:ys)
    sumsOfPairs _ = []                                  -- matching for both [] and [x] arguments


-- Erastosthenes' sieve (sieve [2..100])
sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve (crossOut x xs)

crossOut :: Int -> [Int] -> [Int]
crossOut x ys = [z | z <- ys, z `mod` x /= 0]  -- removes all multiples of x from ys

primes = sieve [2..100]

isPrime x = x `elem` primes
sumOf2Primes s = [(x, y) | x <- primes, y <- primes, x + y == s ]
isSumOf2Primes s = not (null (sumOf2Primes s))

evenNumberNotBeingSumOf2Primes range = [x | x <- range, even x, not (isSumOf2Primes x)]



-- QuickSort
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x::xs) = smallerThanX ++ [x] ++ largerThanX
  where smallerThanX = quickSort [y | y <- xs, y <= x]    -- filter (<=x) xs
        largerThanX  = quickSort [y | y <- xs, y > x]     -- filter (>x) xs


-- List of functions
zipWith (\f (a,b) -> f a b) [(+), (*), (-)] (zip [2,3,10] [2,3,5])

-- Usage of folds
reverse' = foldl (\acc x -> x : acc) []
reverse'' = foldl (flip (:)) []           --tricky ;)
head' = foldr1 (\x _ -> x)                --always use the accumulator (at first initialized to first element value)
last' = foldl1 (\_ x -> x)                --always use the x element ignoring accumulator









