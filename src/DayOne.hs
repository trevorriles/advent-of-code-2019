{-# LANGUAGE TypeApplications #-}

module DayOne where

import Data.Monoid

solve :: IO ()
solve = do
    input <- readFile "./src/day-one-input.txt"
    let d = read <$> lines input :: [Int]
    let fuel = getSum $ foldMap calculateFuel d
    putStrLn $ show fuel

calculateFuel :: Int -> Sum Int
calculateFuel x = Sum $ (quot x 3) - 2


solve2 :: IO ()
solve2 = do
    input <- readFile "./src/day-one-input.txt"
    let d = read @Int <$> lines input
    let fuel = getSum $ foldMap (Sum . calculateFuel2 0) d
    putStrLn $ show fuel


calculateFuel2 :: Int -> Int -> Int
calculateFuel2 total x = 
    if value <= 0 
        then total 
        else calculateFuel2 (total + value) value
    where value = (quot x 3) - 2