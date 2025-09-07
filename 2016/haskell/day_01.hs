#!/usr/bin/env cabal

{- cabal:
    build-depends: base, split
-}

import Data.Complex
import Data.List.Split (splitOn)

problemFilename = "../inputs/day_01.txt"

main :: IO()
main = do
    input <- readFile problemFilename
    let resultProb1 = solverProb1 input
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
    let splits = map (head . words) $ splitOn "," input
    in map (\x -> buildInstruction (head x) (read $ tail x) ) splits

applyInstruction :: Block -> Instruction -> Block
applyInstruction block instruction =
    let
        distance = case instruction of
            L d -> d
            R d -> d
        newDirection = case instruction of
            L _ -> (0 :+ (-1)) * (direction block)
            R _ -> (0 :+ 1) * (direction block)
    in Block { coord = coord block + (distance :+ 0) * newDirection, direction = newDirection }

followInstructions :: Block -> [Instruction] -> Block
followInstructions = foldl applyInstruction

shortestPath :: Block -> Double
shortestPath = manhattanDistance startBlock

solverProb1 :: String -> Double
solverProb1 input = shortestPath $ followInstructions startBlock (parseInstructions input)