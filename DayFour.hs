module DayFour where

import Data.Digits
import Control.Monad
import Data.List

solve :: Int -> Int -> IO ()
solve start stop = do
    let numbers = [start..stop]
    let result = length $ filter test numbers
    putStrLn $ show result

solve2 :: Int -> Int -> IO ()
solve2 start stop = do
    let numbers = [start..stop]
    let result = length $ filter test2 numbers
    putStrLn $ show result

test :: Int -> Bool
test n = do
    let d = digits 10 n
    isSixDigits d && adjacent d && noDecrease d

test2 :: Int -> Bool
test2 n = do
    let d = digits 10 n
    isSixDigits d && adjacent2 d && noDecrease d

isSixDigits :: [Int] -> Bool
isSixDigits = (==6) . length 

adjacent :: [Int] -> Bool
adjacent = any ((>1) . length) . group

adjacent2 :: [Int] -> Bool
adjacent2 = any ((==2) . length) . group
        

noDecrease :: [Int] -> Bool
noDecrease xs = and $ zipWith (<=) xs $ tail xs