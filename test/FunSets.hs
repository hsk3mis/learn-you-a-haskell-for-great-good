import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, (@=?))
import Control.Exception (evaluate, catch)

main = defaultMain unitTestsFunSets

unitTestsFunSets = testGroup "Unit tests FunSets"
  [ ]

{- Functional Sets => Set defined by the membership function -}
type Set = Int -> Bool

{- Indicates whether a set contains a given element -}
contains :: Set -> Int -> Bool


{- Returns the set of the one given element -}
singletonSet :: Int -> Set

{- Returns the union of the two given sets, the sets of all elements that are in either `s` or `t`. -}
union :: Set -> Set -> Set

{- Returns the intersection of the two given sets , the set of all elements that are both in `s` and `t`. -}
intersect :: Set -> Set -> Set

{- Returns the difference of the two given sets, the set of all elements of `s` that are not in `t`. -}
diff :: Set -> Set -> Set

{- Returns the subset of `s` for which predicate `p` holds. -}
filter :: Set -> (Int -> Bool) -> Set


{-
  The bounds for `forall` and `exists` are +/- 1000.
  Needed, because we can't iterate elements of the Set.
  We must iterate elements from -bound to bound and check if element is a member of the Set.
-}
bound = 1000

{- Returns whether all bounded integers within `s` satisfy `p`. -}
forall :: Set -> (Int -> Bool) -> Bool

{- Returns whether there exists a bounded integer within `s` that satisfies `p`. -}
exists :: Set -> (Int -> Bool) -> Bool

{- Returns a set transformed by applying `f` to each element of `s`. -}
map :: Set -> (Int -> Int) -> Set

{- Displays the contents of a set -}
toString :: Set -> String









