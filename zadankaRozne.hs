import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Char as Char
import Data.Function (on)


{- Walidacja a'la PESELU ze składaniem funkcji
  The last digit of a Swedish personal number (in 10 digit form) is a “checksum” digit and is included to make it possible to check whether there might be errors in the other numbers. The algorithm for checking is standard (the Luhn algorithm), and is used for many different kinds of identification numbers of different sizes. The algorithm is described as follows (using the example of the personal number 6504089019)

  From the rightmost digit and moving left, double the value of every second digit (i.e. double the second from last (1), fourth from last (9), etc). In the example, this gives the numbers 6+6, 5, 0+0, 4, 0+0, 8, 9+9, 0, 1+1, 9 = 12,5,0,4,0,8,18,2,9
  Take the sum of all the digits in the result (1+2+5+0+4+0+8+1+8+2+9 = 40). Note that we add together the digits, which is not the same as just adding the numbers!
  If the total is divisible by 10 (as it is in this example), the card number is valid, otherwise it is invalid.
  Define a function valid :: Integer -> Bool and suitable helper functions to check if a number is valid according to the above algorithm. You may assume that the number is positive, but the function should work for any number of digits.

  Hint: digitToInt :: Char -> Int may be useful.
-}

import Data.Char
valid :: String -> Bool
valid = (==0).(`mod` 10).sum.addDigits.doubleSecondFromEnd.count.toDigit

f .> g = g . f
valid2 :: String -> Bool
valid2 = toDigit .> count .> doubleSecondFromEnd .> addDigits .> sum .> (`mod` 10) .> (==0)

test = valid "6504089019" && valid2 "6504089019" && (not $ valid "6504089018") && (not $ valid2 "6504089018")


count :: [Int] -> [(Int, Int, Int)]
count list = zip3 list [1..] $ repeat $ length list
doubleSecondFromEnd ::  [(Int, Int, Int)] -> [Int]
doubleSecondFromEnd = map (\(value, number, allNumbers) -> if (allNumbers - number) `mod` 2 /= 0 then value*2 else value)
addDigits :: [Int] -> [Int]
addDigits = map (sum.digs)
toDigit :: String -> [Int]
toDigit  = map digitToInt

-- copypaste z sieci
digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]
