import Lib (double, half)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, (@=?))
import Control.Exception (evaluate, catch)

main = defaultMain unitTestsChapter1

unitTestsChapter1 = testGroup "Unit tests Chapter1"
  [infixFunctionApplication,
   listsGetElemByIndex, listsConcatenation, listsConstruction, listElementByIndex, listsComparison, listsNullCheck, listsTake, listsElem,
   simpleRangeDefinitionWithStep, decreasingRange, decreasingRangeError, cycleExample, repeatExample,
   listComprehesionExample, listComprehesionMultipleLists, lengthExample, listComprehesionNestedForNestedLists,
   tupleFirstElement, tupleSecondElement, zipListsExample,
   trianglesExample,
   qsortEmptyList, qsortSingleElementList, qsortSortedList, qsortReversedList, qsortUnsortedList,
   noDuplicatesInEmptyList, noDuplicatesInSingleElementList, noDuplicatesInList, duplicatesInList,
   hanoiOf2, hanoiOf3, hanoiOf8, hanoiOf64
  ]

infixFunctionApplication =
  testCase "Infix function application" $ assertEqual [] (div  91 10) (91 `div` 10)

-- Infix function definition
a `max'` b = if (a >= b) then a else b

-- LISTS
listsGetElemByIndex =
  testCase "Lists: get element by index" $ assertEqual [] 'c' (['a'..'z'] !! 2)

listsConcatenation =
  testCase "Lists: concatenation (must iterate the whole first list)" $ assertEqual [] "woot" (['w', 'o'] ++ ['o', 't'])

listsConstruction =
  testCase "Lists: construction (append at the beginning) (instantaneous)" $ (@=?) [0, 1, 2, 3] (0 : [1, 2, 3])

listElementByIndex =
    testCase "Lists: element by index" $ (@=?) 'e' ("hello" !! 1)

listsComparison =
  testCase "Lists: comparision with lexographical order" $ assertBool "" ([2, 2, 7] < [2, 3, 1])

--exceptionOnEmptyList =
--  head [] ==> exception

listsNullCheck =
  testCase "Lists: null check" $ assertBool "" (null [])

listsTake =
  testCase "Lists: take/drop" $ assertEqual [] [5, 6] (take 2 [5, 6, 7])

listsElem =
  testCase "Lists: check element existence" $ assertBool "" (2 `elem` [1, 2, 3, 4])

-- RANGES
simpleRangeDefinitionWithStep =
  testCase "Ranges: simple definition with step" $ assertEqual [] [1, 5, 9, 13] [1,5..13]

decreasingRange =
  testCase "Ranges: decreasing example" $ assertEqual [] [5, 4, 3, 2, 1] [5, 4 .. 1]

decreasingRangeError =
  testCase "Ranges: decreasing example error" $ assertEqual [] [] [5..1]

cycleExample =
  testCase "Ranges: definition by cycle" $ assertEqual [] [1, 2, 3, 1, 2] (take 5 (cycle [1, 2, 3]))

repeatExample =
  testCase "Ranges: definition by reapeating" $ assertEqual [] [5, 5, 5] (take 3 (repeat 5))

-- LIST COMPREHENSIONS
listComprehesionExample =
  testCase "List comprehensions: example with predicates" $ assertEqual [] [12,14,18,20] [x*2 | x <- [1..10], x*2 >= 12, x*2 /= 16]
  -- [ <outputFunction> | <variable> <- <inputSet>, <predicate> ]

listComprehesionMultipleLists =
  testCase "List comprehensions: multiple lists" $ assertEqual [] [55,80,100,110] [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]

lengthExample =
  let length' xs = sum [1 | _ <- xs] in
    testCase "List comprehensions: length definition" $ assertEqual [] 4 (length' [1, 2, 3, 4])

listComprehesionNestedForNestedLists =
  let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]] in
    testCase "List comprehensions: nested" $ assertEqual [] [[2,2,4],[2,4,6,8]] [ [ x | x <- xs, even x ] | xs <- xxs, length xs < 10]

-- TUPLES
tupleFirstElement =
  testCase "Tuples: fst" $ assertEqual [] 5 (fst (5, 10))

tupleSecondElement =
  testCase "Tuples: snd" $ assertEqual [] 10 (snd (5, 10))

zipListsExample =
  testCase "Tuples: zip of lists (shortened to the shortest list)" $ assertEqual [] [(1, "one"), (2, "two"), (3, "three")] (zip [1 ..] ["one", "two", "three"])

trianglesExample =
  let sections = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ] in
    let triangles = [ (a,b,c) | (a, b, c) <- sections, b <= c, a <= b, a^2 + b^2 == c^2] in
      let rightTriangles = [ (a,b,c) | (a, b, c) <- triangles, a+b+c == 24] in
        testCase "Triangles example" $ assertEqual [] [(6, 8, 10)] rightTriangles

-- QUICK SORT
qsort [] = []
qsort (x:xs) = qsort [k | k<-xs, k <= x] ++ [x] ++  qsort [k | k<-xs, k > x]

qsortEmptyList =
  testCase "QuickSort: empty list" $ assertEqual [] [] (qsort ([] ::[Integer]))

qsortSingleElementList =
  testCase "QuickSort: single element list" $ assertEqual [] [1] (qsort [1])

qsortSortedList =
  testCase "QuickSort: sorted list" $ assertEqual [] [1..4] (qsort [1, 2, 3, 4])

qsortReversedList =
  testCase "QuickSort: reversed list" $ assertEqual [] [1..4] (qsort [4, 3, 2, 1])

qsortUnsortedList =
  testCase "QuickSort: unsorted list" $ assertEqual [] [1..4] (qsort [3, 2, 4, 1])


-- AVOID DUPLICATES
duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates (x:xs) = (x `elem` xs) || duplicates xs

noDuplicatesInEmptyList =
  testCase "Duplicates: empty list" $ assertEqual [] False (duplicates ([] :: [Integer]))

noDuplicatesInSingleElementList =
  testCase "Duplicates: single element list" $ assertEqual [] False (duplicates [1])

noDuplicatesInList =
  testCase "Duplicates: no duplicates in list" $ assertEqual [] False (duplicates [1, 3, 2])

duplicatesInList =
  testCase "Duplicates: duplicates in list" $ assertEqual [] True (duplicates [1, 2, 3, 2])


-- TOWERS OF HANOI
hanoi 0 = 0
hanoi 1 = 1
hanoi n = 1 + 2 * hanoi (n - 1)

hanoiOf2 =
  testCase "Hanoi: of 2" $ assertEqual [] 3 (hanoi 2)

hanoiOf3 =
  testCase "Hanoi: of 3" $ assertEqual [] 7 (hanoi 3)

hanoiOf8 =
  testCase "Hanoi: of 8" $ assertEqual [] 255 (hanoi 8)

hanoiOf64 =
  testCase "Hanoi: of 32" $ assertEqual [] (2^32-1) (hanoi 32)


--hanoi4posts n = hanoi4posts (n/2) + hanoi (n/2) + hanoi4posts (n/2)
hanoi4posts 0 = 0
hanoi4posts 1 = 1
hanoi4posts n = bestK (n-1) n

-- BARDZO WOLNE:
-- hanoi4postsK k n = hanoi4posts k + hanoi (n-k) + hanoi4posts k

-- TO JUŻ JEST DUŻO WYDAJNIEJSZE
hanoi4postsK k n =
  let m = hanoi4posts k in
    m + hanoi (n-k) + m

bestK 1 n = hanoi4postsK 1 n
bestK k n = min (hanoi4postsK k n) (bestK (k-1) n)


hanoi4 :: Integer -> Integer
hanoi4 0         = 0
hanoi4 1         = 1
hanoi4 n | n > 0 = bestk 1 n

bestk :: Integer -> Integer -> Integer
bestk k n
  | k == n-1 = steps
  | k < n-1  = min steps (bestk (k+1) n)
 where
  m     = hanoi4 k
  steps = m + hanoi (n-k) + m


{-
  hanoi4 8  = 33
  hanoi4 16 = 161 (szybko)
  hanoi4 20 = 289 (szybko)
  hanoi4 32 = NIE wylicza się

  WOLNA WERSJA (PRZED ZMIANAMI)
  hanoi4posts 8  = szybko
  hanoi4posts 14 = 113 wolno się liczy
  hanoi4posts 15 = 129 wolno sie liczy
  hanoi4posts 16 = 161 baaaardzo wolno się liczy

-}







