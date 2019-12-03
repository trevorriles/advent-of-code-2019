{-# LANGUAGE TypeApplications #-}

module DayThree where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.Function ((&))

data Direction = U | D | L | R deriving (Eq, Show, Read)

data Instruction = Instruction { direction :: Direction, distance :: Int } deriving (Eq, Show)

solve :: IO ()
solve = do
    input <- readFile "./src/day-three-input.txt"
    let (x:y:[]) = lines input
    let result = solve' x y
    putStrLn . show $ result

solve' :: String -> String -> Int
solve' in1 in2 = do
    let set1 = S.fromList $ runInstructions [(0,0)] $ parseInput in1
    let set2 = S.fromList $ runInstructions [(0,0)] $ parseInput in2
    let distances = S.map (\(x, y) -> abs x + abs y) $ S.intersection set1 set2
    S.findMin $ S.filter (> 0) distances

parseInstruction :: String -> Instruction
parseInstruction (d:n) = (read n) & case d of
    'U' -> Instruction U
    'D' -> Instruction D
    'L' -> Instruction L
    'R' -> Instruction R
    _ -> error "Bad input"
parseInstruction _ = error "Bad input"
    
parseInput :: String -> [Instruction]
parseInput input = parseInstruction <$> splitOn "," input

runInstructions :: [(Int, Int)] -> [Instruction] -> [(Int, Int)]
runInstructions state [] = state
runInstructions state (x:xs) = runInstructions (addCoords state x) xs

addCoords :: [(Int, Int)] -> Instruction -> [(Int, Int)]
addCoords state (Instruction _ 0) = state
addCoords state@((x, y) : _) (Instruction d n) = do 
    case d of
        U -> addCoords ((x, y + 1):state) (Instruction d $ n - 1)
        D -> addCoords ((x, y - 1):state) (Instruction d $ n - 1)
        R -> addCoords ((x + 1, y):state) (Instruction d $ n - 1)
        L -> addCoords ((x - 1, y):state) (Instruction d $ n - 1)
addCoords _ _ = error "Bad Input"


solve2 :: IO ()
solve2 = do
    input <- readFile "./src/day-three-input.txt"
    let (x:y:[]) = lines input
    let result = solve2' x y
    putStrLn . show $ result

solve2' :: String -> String -> Int
solve2' in1 in2 = do
    let (_, _, m1) = runInstructions2 $ parseInput in1
    let (_, _, m2) = runInstructions2 $ parseInput in2
    let latencyMap = M.intersectionWith (+) m1 m2
    foldl (\x y -> if x <= y then x else y) 999999 latencyMap


runInstructions2 :: [Instruction] -> (Int, (Int, Int), M.Map (Int, Int) Int)
runInstructions2 = foldl addCoords2 (0, (0,0), mempty)


addCoords2 :: (Int, (Int, Int), M.Map (Int, Int) Int) -> Instruction -> (Int, (Int, Int), M.Map (Int, Int) Int)
addCoords2 state (Instruction _ 0) = state
addCoords2 (steps, (x, y), state) (Instruction d n) = do 
    let newSteps = steps + 1
    case d of
        U -> addCoords2 (newSteps, (x, y + 1), M.insert (x, y + 1) (newSteps) state) (Instruction d $ n - 1)
        D -> addCoords2 (newSteps, (x, y - 1), M.insert (x, y - 1) (newSteps) state) (Instruction d $ n - 1)
        R -> addCoords2 (newSteps, (x + 1, y), M.insert (x + 1, y) (newSteps) state) (Instruction d $ n - 1)
        L -> addCoords2 (newSteps, (x - 1, y), M.insert (x - 1, y) (newSteps) state) (Instruction d $ n - 1)