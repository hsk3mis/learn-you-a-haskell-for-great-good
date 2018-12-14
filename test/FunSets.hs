import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, (@=?))
import Control.Exception (evaluate, catch)

main = defaultMain unitTestsFunSets

unitTestsFunSets = testGroup "Unit tests FunSets"
  [
    testCase "FunSets: singleton set contains it's only element" $ assertEqual [] True (set1 1),
    testCase "FunSets: singleton set doesn't contain other elements" $ assertEqual [] False (set1 2),

    testCase "FunSets: union contains both elements" $ assertEqual [] True $ (set12 1) && (set12 2),
    testCase "FunSets: union don't contain other elements" $ assertEqual [] False $ set12 3,

    testCase "FunSets: intersect contains common elements" $ assertEqual [] True $ let intersectS = intersect set12 set13 in intersectS 1,
    testCase "FunSets: intersect don't contain element of only one source set" $ assertEqual [] False $ let intersectS = intersect set12 set13 in intersectS 2,

    testCase "FunSets: diff contains elements from first set only" $ assertEqual [] True $ let diffS = diff set12 set13 in diffS 2,
    testCase "FunSets: diff don't contain elements from both sets" $ assertEqual [] False $ let diffS = diff set12 set13 in diffS 1,

    testCase "FunSets: forAll positive" $ assertEqual [] True $ forall set12 (\x -> x < 3),
    testCase "FunSets: forAll negative" $ assertEqual [] False $ forall set12 (\x -> x < 2),

    testCase "FunSets: exists positive" $ assertEqual [] True $ exists set12 (\x -> x == 2),
    testCase "FunSets: exists negative" $ assertEqual [] False $ forall set12 (\x -> x == 3),

    testCase "FunSets: map contains transformed elements" $ assertEqual [] True $ let plus10 = map' set12 (+10) in (plus10 11) && (plus10 12),
    testCase "FunSets: map don't contain other elements" $ assertEqual [] False $ let plus10 = map' set12 (+10) in plus10 2,

    testCase "FunSets: toString should just work" $ assertEqual [] "[1,2]" $ toString set12
  ]

{- Functional Sets => Set defined by the membership function -}
type Set = Int -> Bool

{- Indicates whether a set contains a given element -}
contains :: Set -> Int -> Bool
--contains s x = s x
--contains s = s
contains = id

{- Returns the set of the one given element -}
singletonSet :: Int -> Set
--singletonSet x = \y -> if (y == x) then True else False
--singletonSet x = \y -> y == x
--singletonSet x = (==) x
singletonSet = (==) --jak zaaplikujemy do funkcji (==) jeden argument to dostaniemy funkcję która bierze jeden argument i zwraca czy te rzeczy są równe

set1 = singletonSet 1
set2 = singletonSet 2
set3 = singletonSet 3

{- Returns the union of the two given sets, the sets of all elements that are in either `s` or `t`. -}
union :: Set -> Set -> Set --Expanded Type: (Int -> Bool) -> (Int -> Bool) -> (Int -> Bool)
--union s1 s2 = \x -> s1 x || s2 x
--union s1 s2 x = or [s1 x, s2 x] --Works, but not really better
union s1 s2 x = s1 x || s2 x

set12 = union set1 set2
set13 = union set1 set3

{- Returns the intersection of the two given sets , the set of all elements that are both in `s` and `t`. -}
intersect :: Set -> Set -> Set
intersect s1 s2 = \x -> (s1 x) && (s2 x)

{- Returns the difference of the two given sets, the set of all elements of `s` that are not in `t`. -}
diff :: Set -> Set -> Set
diff s1 s2 = \x -> (s1 x) && (not (s2 x))

{- Returns the subset of `s` for which predicate `p` holds. -}
filter' :: Set -> (Int -> Bool) -> Set
filter' s p = \x -> (s x) && (p x)

{-
  The bounds for `forall` and `exists` are +/- 1000.
  Needed, because we can't iterate elements of the Set.
  We must iterate elements from -bound to bound and check if element is a member of the Set.
-}
bound = 1000

{- Returns whether all bounded integers within `s` satisfy `p`. -}
forall :: Set -> (Int -> Bool) -> Bool
forall s p = [x | x <- [-bound..bound], s x, not (p x)] == [] -- Lazily check list equality (stops when first list already generated 1st element)!
--forall s p = and (map p (filter s [-bound..bound])) -- Also Works

{- Returns whether there exists a bounded integer within `s` that satisfies `p`. -}
exists :: Set -> (Int -> Bool) -> Bool
exists s p = not (forall s (\x -> not (p x)))

{- Returns a set transformed by applying `f` to each element of `s`. -}
map' :: Set -> (Int -> Int) -> Set
map' s f = \x -> exists s (\y -> f y == x)

{- Displays the contents of a set -}
toString :: Set -> String
toString s = show [x | x <- [-bound..bound], s x]