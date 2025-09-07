#!/usr/bin/env cabal

{- cabal:
build-depends: base, split
-}

import Data.Complex
import Data.List.Split (splitOn)
import Data.Char (isSpace)

problemFilename = "../inputs/day_01.txt"

main :: IO()
main = do
    input <- readFile problemFilename
    let resultProb1 = solver input
    putStrLn $ "Problem 1: " ++ (show resultProb1)

data Block = Block {
    coord :: Complex Double,
    direction :: Complex Double
}
data Instruction = L Double | R Double

buildInstruction :: Char -> Double -> Instruction
buildInstruction 'R' distance = R distance
buildInstruction 'L' distance = L distance

startBlock :: Block
startBlock = Block { coord = (0 :+ 0), direction = (0 :+ 1) }

manhattanDistance :: Block -> Block -> Double
manhattanDistance (Block l _) (Block r _)= abs (imagPart l - imagPart r) + abs (realPart l - realPart r)

parseInstructions :: String -> [Instruction]
parseInstructions input =
    let
        splits = map (\x -> dropWhile isSpace x) $ splitOn "," input
    in map (\x -> buildInstruction (head x) (read $ tail x) ) splits

applyInstruction :: Instruction -> Block -> Block
applyInstruction instruction block = 
    let (newDirection, distance) =
            case instruction of
                L distance -> ((0 :+ (-1)) * (direction block), distance)
                R distance -> ((0 :+ 1) * (direction block), distance)
    in Block { coord = coord block + (distance :+ 0) * newDirection, direction = newDirection }

followInstructions :: [Instruction] -> Block -> Block
followInstructions (instruction:xs) block = followInstructions xs $ applyInstruction instruction block
followInstructions [] block = block

shortestPath :: Block -> Double
shortestPath targetBlock = manhattanDistance startBlock targetBlock

solver :: String -> Double
solver input = shortestPath $ followInstructions (parseInstructions input) startBlock