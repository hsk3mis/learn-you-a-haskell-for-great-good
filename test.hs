transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' xss
  | all null xss = []
  | otherwise    = heads xss : (transpose' (tails xss))
  where
    heads xss = [x | (x:_)<-xss]
    tails xss = [xs | (_:xs)<-xss]

--transpose' :: [[a]] -> [[a]]
--transpose' [] = []
--transpose' [xs] = [[x] | x<-xs]
--transpose' (xs:xss) = zip2Lists [[x] | x<-xs] (transpose' xss)
--zip2Lists xs ys = map (\(x,y) -> x ++ y) (zip xs ys)


--transpose'               :: [[a]] -> [[a]]
--transpose' []             = []
--transpose' ([]   : xss)   = transpose' xss
--transpose' ((x:xs):xss)   = (x : [h | (h:_)<-xss]) : (transpose' (xs : [t | (_:t)<-xss]))








la = [[1], [2]]
lb = [[3], [4]]


l = [[1,2],[3,4]]
l2 = [[1,2,3], [4,5,6], [7,8,9]]

test = test1 && test2 && test3

test1 = transpose' [[1,2],[3,4]] == [[1,3], [2,4]]
test2 = transpose' [[1,2,3], [4,5,6], [7,8,9]] == [[1,4,7], [2,5,8], [3,6,9]]
test3 = transpose' [[1, 2, 3]] == [[1], [2], [3]]