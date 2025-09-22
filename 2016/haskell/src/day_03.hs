problemFilename :: String
problemFilename = "../inputs/day_03.txt"

main :: IO ()
main = do
  input <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 input

parse :: String -> [[Int]]
parse input = map (map read . words) (lines input)

solverProb1 :: String -> String
solverProb1 input = show $ length $ filter id $ map (\ [a, b, c] -> isTriangle a b c ) (parse input)

isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c = (a + b > c) && (a + c> b) && (b + c > a)