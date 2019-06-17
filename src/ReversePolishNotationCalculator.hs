-- Execute with: runhaskell ReversePolishNotationCalculator.hs
-- Example "10 4 3 + 2 * -" should result in -4

main = do
  putStrLn "Write expression:"
  expression <- getLine
  let result = solveRPN expression
  putStrLn (show result)
  return result


solveRPN :: (Num a, Read a) => String -> a
--solveRPN expression = head (foldl operatorHandler [] (words expression))  -- non point-free style
solveRPN = head . foldl operatorHandler [] . words                          -- point-free style
  where operatorHandler :: (Num a, Read a) => [a] -> String -> [a]
        operatorHandler (x:y:ys) "+" = (y + x)  :ys
        operatorHandler (x:y:ys) "-" = (y - x)  :ys
        operatorHandler (x:y:ys) "*" = (y * x)  :ys
        operatorHandler xs num       = read num :xs                         -- read converts String to any type; Num in our example

-- Exception handling => return Maybe a
-- check if read can parse a String => use reads eg. reads "123" :: [(Integer,String)] if it returns [] then it's an error, otherwise it returns [(123,"")]



{- Sidelnik -}
calculateRPN rpm = foldl (\acc e -> foo (lookup e operations) acc e) [] (words rpm)

-- założenie, że każda operacja ma 2 argumenty - mozna trzymać liczbę argumentów w tablicy z funkcją
foo Nothing list e = (read e :: Float):list
foo (Just operation) (x:xs:list) _ = (operation xs x):list

-- lista funkcji łatwo rozszerzalna bez zmian istniejących implementacji
operations :: Fractional a => [(String, a -> a -> a)]
operations = [("+", (+)), ("-", (-)), ("*", (*)), ("/", (/))]







