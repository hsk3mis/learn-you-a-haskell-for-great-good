import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, (@=?))
import Control.Exception (evaluate, catch)

main = defaultMain unitTestsFunSets

unitTestsFunSets = testGroup "Unit tests FunSets"
  [
    singletonSetContainsItsOnlyElement, singletonSetDontContainOtherElements,
    unionContainsBothElements, unionDontContainOtherElements,
    intersectContainsCommonElements, intersectDontContainElementOfOnlyOneSourceSet,
    diffContainsElementsFromFirstSetOnly, diffDontContainElementsFromBothSets,
    forAllPositiveCase, forAllNegativeCase,
    existsPositiveCase, existsNegativeCase,
    mapContainsTransformedElements, mapDontContainOtherElements,
    toStringDisplayInArraySyntaxWithoutSpaces
  ]

{- Functional Sets => Set defined by the membership function -}
type Set = Int -> Bool

{- Indicates whether a set contains a given element -}
contains :: Set -> Int -> Bool
contains s x = s x


{- Returns the set of the one given element -}
singletonSet :: Int -> Set
singletonSet x = \y -> if (y == x) then True else False

set1 = singletonSet 1
set2 = singletonSet 2
set3 = singletonSet 3

singletonSetContainsItsOnlyElement =
  testCase "FunSets: singleton set contains it's only element" $ assertEqual [] True (contains set1 1)

singletonSetDontContainOtherElements =
  testCase "FunSets: singleton set doesn't contain other elements" $ assertEqual [] False (contains set1 2)


{- Returns the union of the two given sets, the sets of all elements that are in either `s` or `t`. -}
union :: Set -> Set -> Set
union s1 s2 = \x -> s1 x || s2 x

set12 = union set1 set2
set13 = union set1 set3

unionContainsBothElements =
  testCase "FunSets: union contains both elements" $ assertEqual [] True $ (contains set12 1) && (contains set12 2)

unionDontContainOtherElements =
  testCase "FunSets: union don't contain other elements" $ assertEqual [] False $ contains set12 3

{- Returns the intersection of the two given sets , the set of all elements that are both in `s` and `t`. -}
intersect :: Set -> Set -> Set
intersect s1 s2 = \x -> (s1 x) && (s2 x)

intersectContainsCommonElements =
  testCase "FunSets: intersect contains common elements" $ assertEqual [] True $ contains intersectS 1
    where intersectS = intersect set12 set13

intersectDontContainElementOfOnlyOneSourceSet =
  testCase "FunSets: intersect don't contain element of only one source set" $ assertEqual [] False $ contains intersectS 2
    where intersectS = intersect set12 set13

{- Returns the difference of the two given sets, the set of all elements of `s` that are not in `t`. -}
diff :: Set -> Set -> Set
diff s1 s2 = \x -> (s1 x) && (not (s2 x))

diffContainsElementsFromFirstSetOnly =
  testCase "FunSets: diff contains elements from first set only" $ assertEqual [] True $ contains diffS 2
    where diffS = diff set12 set13

diffDontContainElementsFromBothSets =
  testCase "FunSets: diff don't contain elements from both sets" $ assertEqual [] False $ contains diffS 1
    where diffS = diff set12 set13

{- Returns the subset of `s` for which predicate `p` holds. -}
filter :: Set -> (Int -> Bool) -> Set
filter s p = \x -> (s x) && (p x)


{-
  The bounds for `forall` and `exists` are +/- 1000.
  Needed, because we can't iterate elements of the Set.
  We must iterate elements from -bound to bound and check if element is a member of the Set.
-}
bound = 1000

{- Returns whether all bounded integers within `s` satisfy `p`. -}
forall :: Set -> (Int -> Bool) -> Bool
forall s p = [x | x <- [-bound..bound], s x, not (p x)] == []

forAllPositiveCase =
  testCase "FunSets: forAll positive" $ assertEqual [] True $ forall set12 (\x -> x < 3)

forAllNegativeCase =
  testCase "FunSets: forAll negative" $ assertEqual [] False $ forall set12 (\x -> x < 2)

{- Returns whether there exists a bounded integer within `s` that satisfies `p`. -}
exists :: Set -> (Int -> Bool) -> Bool
exists s p = not (forall s (\x -> not (p x)))

existsPositiveCase =
  testCase "FunSets: exists positive" $ assertEqual [] True $ exists set12 (\x -> x == 2)

existsNegativeCase =
  testCase "FunSets: exists negative" $ assertEqual [] False $ forall set12 (\x -> x == 3)

{- Returns a set transformed by applying `f` to each element of `s`. -}
map' :: Set -> (Int -> Int) -> Set
map' s f = \x -> exists s (\y -> f y == x)

mapContainsTransformedElements =
  testCase "FunSets: map contains transformed elements" $ assertEqual [] True $ (squares12 1) && (squares12 4)
    where squares12 = map' set12 (\x -> x^2)

mapDontContainOtherElements =
  testCase "FunSets: map don't contain other elements" $ assertEqual [] False $ squares12 2
    where squares12 = map' set12 (\x -> x^2)


{- Displays the contents of a set -}
toString :: Set -> String
toString s = show [x | x <- [-bound..bound], s x]

toStringDisplayInArraySyntaxWithoutSpaces =
  testCase "FunSets: toString should just work" $ assertEqual [] "[1,2]" $ toString set12
