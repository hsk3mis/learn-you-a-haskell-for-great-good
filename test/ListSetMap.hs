import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, (@=?))
import Data.List
import Data.Function (on)

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
