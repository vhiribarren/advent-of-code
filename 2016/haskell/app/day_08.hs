import Data.Set (Set)
import qualified Data.Set as Set

problemFilename :: String
problemFilename = "../inputs/day_08.txt"

type Screen = Set (Int, Int)

screenWidth, screenHeight :: Int
screenWidth = 50
screenHeight = 6

main :: IO ()
main = do
  input <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 input
  putStrLn $ "Problem 2:\n" ++ solverProb2 input

solverProb1, solverProb2 :: String -> String
solverProb1 = show . countLights . computeScreenResult
solverProb2 = displayScreen . computeScreenResult

computeScreenResult :: String -> Screen
computeScreenResult = applyInstructions . map words . lines

applyInstructions :: [[String]] -> Screen
applyInstructions = foldl (flip applyInstruction) Set.empty

applyInstruction :: [String] -> Screen -> Screen
applyInstruction ["rect", r] = let (a, b) = break (== 'x') r in rect (read a, read $ tail b)
applyInstruction ["rotate", "row", row, _, num] = rotateRow (read $ drop 2 row) $ read num
applyInstruction ["rotate", "column", col, _, num] = rotateColumn (read $ drop 2 col) $ read num
applyInstruction _ = error "should not happen"

rect :: (Int, Int) -> Screen -> Screen
rect (x, y) = Set.union $ Set.fromList [(a, b) | a <- [0 .. x - 1], b <- [0 .. y - 1]]

rotateRow :: Int -> Int -> Screen -> Screen
rotateRow row shift screen =
  let lights = [light | a <- [0 .. screenWidth], let light = (a, row), light `elem` screen]
      newLights = [((x + shift) `mod` screenWidth, y) | (x, y) <- lights]
   in Set.union (Set.difference screen $ Set.fromList lights) $ Set.fromList newLights

rotateColumn :: Int -> Int -> Screen -> Screen
rotateColumn col shift screen =
  let lights = [light | b <- [0 .. screenHeight], let light = (col, b), light `elem` screen]
      newLights = [(x, (y + shift) `mod` screenHeight) | (x, y) <- lights]
   in Set.union (Set.difference screen $ Set.fromList lights) $ Set.fromList newLights

countLights :: Screen -> Int
countLights = Set.size

displayScreen :: Screen -> String
displayScreen screen = unlines $ [[if (x, y) `elem` screen then '#' else ' ' | x <- [0 .. screenWidth - 1]] | y <- [0 .. screenHeight - 1]]