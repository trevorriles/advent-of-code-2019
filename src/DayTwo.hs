{-# LANGUAGE TypeApplications #-}

module DayTwo where

import Data.List.Split (splitOn)
import Control.Lens

solve :: IO ()
solve = do
    input <- readFile "./src/day-two-input.txt"
    let codes = read @Int <$> splitOn "," input
    let codes' = codes & element 1 .~ 12 & element 2 .~ 2
    putStrLn $ show $ compute codes' 0

compute :: [Int] -> Int -> [Int]
compute input pos = do
    let cmd = input ^?! element pos
    case cmd of
        99 -> input
        1 -> do
            let in1 = input ^?! element (input ^?! element (pos + 1))
            let in2 = input ^?! element (input ^?! element (pos + 2))
            let out = input ^?! element (pos + 3)
            compute (input & element out .~ (in1 + in2)) (pos + 4)
        2 -> do
            let in1 = input ^?! element (input ^?! element (pos + 1))
            let in2 = input ^?! element (input ^?! element (pos + 2))
            let out = input ^?! element (pos + 3)
            compute (input & element out .~ (in1 * in2)) (pos + 4)
        _ -> error "Bad input"


solve2 :: Int -> Int -> IO ()
solve2 noun verb = do
    input <- readFile "./src/day-two-input.txt"
    let codes = read @Int <$> splitOn "," input
    let codes' = codes & element 1 .~ noun & element 2 .~ verb
    let output = compute codes' 0 ^?! element 0
    if output == 19690720 then 
        putStrLn $ show (100 * noun + verb)
    else
        if verb < 99 then
            solve2 noun (verb + 1)
        else if noun < 99 then
            solve2 (noun + 1) 0
        else
            putStrLn "Failure to solve puzzle"