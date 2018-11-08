module Lib
    ( someFunc, double, half) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

double x = x * 2
half x = x / 2
