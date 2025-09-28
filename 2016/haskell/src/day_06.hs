import Data.List (sort, group, sortOn, transpose)
problemFilename :: String
problemFilename = "../inputs/day_06.txt"

main :: IO ()
main = do
  input <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 input
  putStrLn $ "Problem 2: " ++ solverProb2 input

solverProb1, solverProb2 :: String -> String
solverProb1 = map mostOccurences . transpose . lines
solverProb2 = map leastOccurences . transpose . lines

mostOccurences, leastOccurences :: [Char] -> Char
mostOccurences = head . last . sortOn length . group . sort
leastOccurences = head . head . sortOn length . group . sort