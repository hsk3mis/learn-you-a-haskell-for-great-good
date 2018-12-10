--module Lib
--    ( someFunc, double, half, prop_Sorted, testSorted) where
module Lib where

import Test.QuickCheck

someFunc :: IO ()
someFunc = putStrLn "someFunc"

double x = x * 2
half x = x / 2

-- Chapter 1 => Starting Out (functions, ranges, list comprehensions, tuple)
doubleUs x y = doubleMe x + doubleMe y
doubleMe x = x + x






-- TESTING OF QUICK CHECK -> it works and return IO () printing "+++ OK, passed 100 tests."
-- executes 100 tests with random values for prop_Sorted
sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:tail@(y:ys)) = (x <= y) && (sorted tail)

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

prop_Sorted :: [Integer] -> Bool
prop_Sorted xs = sorted (insertionSort xs)

testSorted = quickCheck prop_Sorted



-- DEFINING CUSTOM DATA TYPES TEST
data T a = N | a :-> (T a) deriving (Show)
infixr :->

mylist :: T Int
mylist = 10 :-> 17 :-> N

mylistSize N = 0
mylistSize (x :-> xs) = 1 + mylistSize xs


{------------------------------------------------------------------------------------}



