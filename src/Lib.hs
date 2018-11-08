module Lib
    ( someFunc, double, half) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

double x = x * 2
half x = x / 2

-- Chapter 1 => Starting Out (functions, ranges, list comprehensions, tuple)
doubleUs x y = doubleMe x + doubleMe y
doubleMe x = x + x

