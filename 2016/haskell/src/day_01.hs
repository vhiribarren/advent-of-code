import Data.Complex
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

problemFilename :: String
problemFilename = "../inputs/day_01.txt"

main :: IO ()
main = do
  input <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ show (solverProb1 input)
  putStrLn $ "Problem 2: " ++ show (solverProb2 input)

data Block = Block
  { coord :: Complex Double,
    direction :: Complex Double
  }
  deriving (Eq)

data Instruction = L Double | R Double

buildInstruction :: Char -> Double -> Instruction
buildInstruction 'R' distance = R distance
buildInstruction 'L' distance = L distance
buildInstruction _ _ = undefined

startBlock :: Block
startBlock = Block {coord = 0 :+ 0, direction = 0 :+ 1}

manhattanDistance :: Block -> Block -> Double
manhattanDistance (Block l _) (Block r _) = abs (imagPart l - imagPart r) + abs (realPart l - realPart r)

parseInstructions :: String -> [Instruction]
parseInstructions input =
  let splits = map (head . words) $ splitOn "," input
  in map (\x -> buildInstruction (head x) (read $ tail x)) splits

applyInstruction :: Block -> Instruction -> [Block]
applyInstruction block instruction =
  let distance = case instruction of
        L d -> d
        R d -> d
      newDirection = case instruction of
        L _ -> (0 :+ (-1)) * direction block
        R _ -> (0 :+ 1) * direction block
   in [Block {coord = coord block + (d :+ 0) * newDirection, direction = newDirection} | d <- [1 .. distance]]

followInstructions :: Block -> [Instruction] -> Block
followInstructions = foldl (\b i -> last (applyInstruction b i))

shortestPath :: Block -> Double
shortestPath = manhattanDistance startBlock

solverProb1 :: String -> Double
solverProb1 input = shortestPath $ followInstructions startBlock (parseInstructions input)

addVisitedBlocks :: Set (Int, Int) -> [Block] -> Set (Int, Int)
addVisitedBlocks set blocks =
  let coords = map (\b -> (round $ realPart $ coord b, round $ imagPart $ coord b)) blocks
  in set `Set.union` Set.fromList coords


hasVisitedBlock :: Set (Int, Int) -> Block -> Bool
hasVisitedBlock set block =
  let coords = coord block
      x = round $ realPart coords
      y = round $ imagPart coords
   in Set.member (x, y) set

followInstructionsWithFirstLocationStop :: Block -> [Instruction] -> Block
followInstructionsWithFirstLocationStop initBlock instructions =
  let nextInstruction visited block (instruction : nextInstructions) =
        let blocksInPath =  applyInstruction block instruction
            visitedBlock = find (hasVisitedBlock visited) blocksInPath
            lastBlock = last blocksInPath
        in case visitedBlock of
          Just b -> b
          Nothing -> nextInstruction (addVisitedBlocks visited blocksInPath) lastBlock nextInstructions
      nextInstruction _ block [] = block
   in nextInstruction Set.empty initBlock instructions

solverProb2 :: String -> Double
solverProb2 input = shortestPath $ followInstructionsWithFirstLocationStop startBlock (parseInstructions input)