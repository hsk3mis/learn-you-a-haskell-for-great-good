import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, (@=?))
import Data.List

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
    testCase "iterate - applies function multiple times" $ assertEqual [] ("hey", "man") $ splitAt 3 "heyman"

  ]


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