-- Execute with: runhaskell HeathrowToLondon.hs

main = do
  input <- getContents
  let crosses = readRoadSystem input
  let Path labels weight = shortestPath crosses
  putStr "The best path to take is: "
  putStrLn $ show $ reverse labels
  putStr "The price is: "
  putStrLn $ show weight

input = "50\n10\n30\n5\n90\n20\n40\n2\n25\n10\n8\n0"
--output => The best path to take is: BCACBBC
--       => The price is: 75
crosses = readRoadSystem input
result = shortestPath crosses

heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

{- Crossroad with values for A road, B road, crossing between A and B -}
data Section = Section Int Int Int deriving (Show)
type RoadSystem = [Section]

data Path = Path [Label] Int deriving (Show, Eq)
instance Ord Path where
  compare (Path _ a) (Path _ b) = compare a b

data Label = A | B | C deriving (Show, Eq)
--type Path = [(Label, Int)]

{- Uwaga: nie można utowrzyć instancji typeclass'y Read dla synonimu RoadSystem -}
readRoadSystem :: String -> RoadSystem
readRoadSystem = map (\[a,b,c] -> Section a b c) . splitEvery 3 . map (\x -> read x) . lines

-- inna nazwa to może być "groupsOf"
splitEvery :: Int -> [a] -> [[a]]
splitEvery 0 _  = undefined
splitEvery _ [] = []
splitEvery n xs = firstN : splitEvery n theRest
  where (firstN, theRest) = splitAt n xs
--splitEvery n xs = take n xs : splitEvery n (drop n xs)


{- można składać odwróconą ścieżkę dokładając na początek przy pomocy : lub normalnie przy pomocy ++ -}
shortestPath :: [Section] -> Path
shortestPath crosses = min minA minB
  where (minA, minB) = foldl handler (Path [] 0, Path [] 0) crosses
        handler :: (Path, Path) -> Section -> (Path, Path)
        handler (Path minPathA weightA, Path minPathB weightB) (Section a b c) =
          let newMinPathWithWeightA = if (weightA + a <= weightB + b + c) then Path (A : minPathA) (weightA + a) else Path (C:B:minPathB) (weightB + b + c) in
          let newMinPathWithWeightB = if (weightB + b <= weightA + a + c) then Path (B : minPathB) (weightB + b) else Path (C:A:minPathA) (weightA + a + c) in
            (newMinPathWithWeightA, newMinPathWithWeightB)




