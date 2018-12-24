import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, (@=?))
import Data.List
import Data.Function (on)
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

main = defaultMain unitTestsListSetMap

unitTestsListSetMap = testGroup "Unit tests Lists, Sets, Maps etc."
  [
    testCase "intersparse" $ assertEqual [] "M.O.N.K.E.Y" $ intersperse '.' "MONKEY",
    testCase "intercalate" $ assertEqual [] "hey there guys" $ intercalate " " ["hey","there","guys"],

    testCase "transpose (proper matrix)" $ assertEqual [] [[1,4,7],[2,5,8],[3,6,9]] $ transpose [[1,2,3],[4,5,6],[7,8,9]],
    testCase "transpose (not matrix)" $ assertEqual [] [[1,3,5],[2,4,6],[7]] $ transpose [[1,2],[3,4],[5,6,7]],
    testCase "transpose' (proper matrix)" $ assertEqual [] [[1,4,7],[2,5,8],[3,6,9]] $ transpose' [[1,2,3],[4,5,6],[7,8,9]],
    testCase "transpose' (not matrix)" $ assertEqual [] [[1,3,5],[2,4,6],[7]] $ transpose' [[1,2],[3,4],[5,6,7]],

    testCase "concat" $ assertEqual [] "foobarcar" $ concat ["foo","bar","car"],
    testCase "concatMap" $ assertEqual [] [1,1,1,1,2,2,2,2,3,3,3,3] $ concatMap (replicate 4) [1..3],
    testCase "and" $ assertEqual [] True $ and $ map (>4) [5,6,7,8],
    testCase "any" $ assertEqual [] True $ any (==4) [2,3,5,6,1,4],
    testCase "all" $ assertEqual [] False $ all (`elem` ['A'..'Z']) "HEYGUYSwhatsup",
    testCase "iterate - applies function multiple times" $ assertEqual [] [1,2,4,8,16,32,64,128,256,512] $ take 10 $ iterate (*2) 1,
    testCase "iterate - applies function multiple times" $ assertEqual [] ("hey", "man") $ splitAt 3 "heyman",

    testCase "takeWhile" $ assertEqual [] [1..13] $ takeWhile (<14) [1..],
    testCase "dropWhile" $ assertEqual [] " is a sentence" $ dropWhile (/=' ') "This is a sentence",

    testCase "span" $ assertEqual [] ([1..13], [14..100]) $ span (<14) [1..100],
    testCase "break" $ assertEqual [] ([1,2,3],[4,5,6,7]) $ break (==4) [1,2,3,4,5,6,7],
    {- break p == span (not . p) -}
    testCase "break" $ assertEqual [] True $ (break (==4) [1,2,3,4,5,6,7]) == span (/=4) [1,2,3,4,5,6,7],

    testCase "group: groups adjacent elements into sublists if they are equal" $ assertEqual [] [[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]] $ group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7],

    testCase "inits" $ assertEqual [] ["","w","w0","w00","w00t"] $ inits "w00t",
    testCase "tails" $ assertEqual [] ["w00t","00t","0t","t",""] $ tails "w00t",
    testCase "init and tails" $ assertEqual [] [("","w00t"),("w","00t"),("w0","0t"),("w00","t"),("w00t","")] $ let w = "w00t" in zip (inits w) (tails w),

    testCase "isInfixOf" $ assertEqual [] True $ "cat" `isInfixOf` "im a cat burglar",
    testCase "isPrefixOf" $ assertEqual [] True $ "hey" `isPrefixOf` "hey there!",
    testCase "isSuffixOf" $ assertEqual [] True $ "there!" `isSuffixOf` "oh hey there!",

    testCase "elem" $ assertEqual [] True $ 'a' `elem` "Ala ma kota",
    testCase "notElem" $ assertEqual [] True $ 'b' `notElem` "Ala ma kota",

    {- partitions list onto 2 groups using given predicate (span and break partition on the first occurance of the predicate) -}
    testCase "partition" $ assertEqual [] ("BOBMORGAN","sidneyeddy") $ partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy",

    testCase "lines" $ assertEqual [] ["first line","second line","third line"] $ lines "first line\nsecond line\nthird line",
    testCase "words" $ assertEqual [] ["a", "b", "c"] $ words "a b c",
    
    testCase "nub" $ assertEqual [] [1,3,2] $ nub [1,1,3,2,2,1],

    testCase "list difference" $ assertEqual [] [1,3,4,6,7,8,10] $ [1..10] \\ [2,5,9],
    {- duplicates removed ONLY from the second list ;) -}
    testCase "union" $ assertEqual [] [1..7] $ [1..5] `union` [3..7],

    {- xxxBy functions uses given equality function instead of == -}
    testCase "groupBy" $ assertEqual [] [[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]] $ groupBy (\x y -> (x > 0) == (y > 0)) [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3],
    {- Defines equality with on function -}
    {- (==) `on` (> 0)  ====  \x y -> (x > 0) == (y > 0) -}
    testCase "groupBy with on" $ assertEqual [] [[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]] $ groupBy ((==) `on` (> 0)) [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3],
    {- compare `on` length  ====   \x y -> length x `compare` length y -}
    testCase "sortBy" $ assertEqual [] [[],[2],[2,2],[1,2,3],[3,5,4,3],[5,4,5,4,4]] $ sortBy (compare `on` length) [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]

    {- GENERAL RULE: with xxxBy functions:
        * by functions that take an equality function, you usually do (==) `on` something
        * when you're dealing with By functions that take an ordering function, you usually do compare `on` something
    -}

    , testCase "on': compare normal" $ assertEqual [] LT $ [1,2,3] `compare` [4,5,6]
    , testCase "on': compare `on'` length" $ assertEqual [] EQ $ let compareOnLength = compare `on'` length in [1,2,3] `compareOnLength` [4,5,6]

    , testCase "ord: A = 65" $ assertEqual [] 65 $ ord 'A'
    , testCase "chr: a = 97" $ assertEqual [] 'a' $ chr 97

    , testCase "caesar cipher: encode" $ assertEqual [] "bcde" $ caesarEncode 1 "abcd"
    , testCase "caesar cipher: decode" $ assertEqual [] "abcd" $ caesarDecode 2 "cdef"

    {- MAPS (Association Lists) -}
    {- Duplicates replace previous value -}
    , testCase "map: find Just b" $ assertEqual [] (Just "c") $ 2 `Map.lookup` (Map.fromList [(1, "a"), (2, "b"), (2, "c")])
    , testCase "map: find Nothing" $ assertEqual [] Nothing $ Map.lookup 3 (Map.fromList [(1, "a"), (2, "b")])
    {- Duplicates are handled with function -}
    , testCase "map: fromListWith concatenation" $ assertEqual [] (Just "cb") $ Map.lookup 2 (Map.fromListWith (\x y -> x ++ y) [(1, "a"), (2, "b"), (2, "c")])
    , testCase "map: fromListWith max" $ assertEqual [] (Just 19) $ Map.lookup 2 (Map.fromListWith max [(1, 11), (2, 12), (2, 19)])

    {- SETS -}
    , testCase "set: intersection" $ assertEqual [] (Set.fromList "abc") $ Set.intersection (Set.fromList "xyzabcqwe") (Set.fromList "poiabclkj")
    , testCase "set: subset is a proper subset or the same set" $ assertEqual [] True $ let s = Set.fromList "abc" in s `Set.isSubsetOf` s
    , testCase "set: proper subset" $ assertEqual [] False $ let s = Set.fromList "abc" in s `Set.isProperSubsetOf` s
    , testCase "set: faster Nub - require Eq (List.nub require only Eq)" $ assertEqual [] "ab" $ setNub "aaaabbbaabbb"





  ]




{- @@ = function like (right-associative with lowest precedence) -}
(@@) :: (a -> b) -> a -> b
f @@ x =  f x
infixr 0 @@

{-iii = same but with normal name-}
iii :: (a -> b) -> a -> b
f `iii` x =  f x
infixr 0 `iii`



-- transpose - custom implementations
transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' xss
  | all null xss = []
  | otherwise    = heads xss : (transpose' (tails xss))
  where
    heads xss = [x | (x:_)<-xss]
    tails xss = [xs | (_:xs)<-xss]

-- Using map and zip
--transpose' [] = []
--transpose' [xs] = [[x] | x<-xs]
--transpose' (xs:xss) = zip2Lists [[x] | x<-xs] (transpose' xss)
--zip2Lists xs ys = map (\(x,y) -> x ++ y) (zip xs ys)

-- Original implementation:
--transpose' []             = []
--transpose' ([]   : xss)   = transpose' xss
--transpose' ((x:xs):xss)   = (x : [h | (h:_)<-xss]) : (transpose' (xs : [t | (_:t)<-xss]))


{- Sarching in the list -}
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)


{- Compare on length-}
on' :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on' comp mapper = \x y -> mapper x `comp` mapper y

{- Caesar cipher -}
caesarEncode :: Int -> String -> String
caesarEncode shift msg = map (chr . (+ shift). ord) msg
--caesarEncode shift msg =
--    let ords = map ord msg
--        shifted = map (+ shift) ords
--    in  map chr shifted

caesarDecode :: Int -> String -> String
caesarDecode shift msg = caesarEncode (negate shift) msg

{- Faster deduplication than List.nub, but require Ord (List.nub only require Eq) -}
setNub xs = Set.toList $ Set.fromList xs