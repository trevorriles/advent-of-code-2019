{-# LANGUAGE TypeApplications #-}

module DayThree where

import Data.Set (Set)
import qualified Data.Set as S
import Data.List.Split (splitOn)

data Direction = U | D | L | R deriving (Eq, Show)

data Instruction = Instruction { direction :: Direction, distance :: Int } deriving (Eq, Show)

solve :: IO ()
solve = do
    input <- readFile "./day-three-input.txt"
    let (x:y:[]) = lines input
    let result = solve' x y
    putStrLn . show $ result

solve' :: String -> String -> Int
solve' in1 in2 = do
    let set1 = S.fromList $ runInstructions [(0,0)] $ parseInput in1
    let set2 = S.fromList $ runInstructions [(0,0)] $ parseInput in2
    let intersect = S.intersection set1 set2
    let distances = S.map (\(x, y) -> abs x + abs y) intersect
    S.findMin $ S.filter (> 0) distances

parseInstruction :: String -> Instruction
parseInstruction (dir:dist) = case dir of
    'U' -> Instruction U $ read @Int dist
    'D' -> Instruction D $ read @Int dist
    'L' -> Instruction L $ read @Int dist
    'R' -> Instruction R $ read @Int dist

parseInput :: String -> [Instruction]
parseInput input = parseInstruction <$> splitOn "," input

runInstructions :: [(Int, Int)] -> [Instruction] -> [(Int, Int)]
runInstructions state [] = state
runInstructions state (x:xs) = runInstructions (addCoords state x) xs

addCoords :: [(Int, Int)] -> Instruction -> [(Int, Int)]
addCoords state (Instruction _ 0) = state
addCoords state@((x, y) : _) (Instruction direction distance) = do 
    case direction of
        U -> addCoords ((x, y + 1):state) (Instruction direction $ distance - 1)
        D -> addCoords ((x, y - 1):state) (Instruction direction $ distance - 1)
        R -> addCoords ((x + 1, y):state) (Instruction direction $ distance - 1)
        L -> addCoords ((x - 1, y):state) (Instruction direction $ distance - 1)