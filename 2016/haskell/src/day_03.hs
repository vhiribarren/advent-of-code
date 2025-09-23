import Data.List.Split (chunksOf)

problemFilename :: String
problemFilename = "../inputs/day_03.txt"

main :: IO ()
main = do
  input <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 input
  putStrLn $ "Problem 2: " ++ solverProb2 input

isTriangle :: [Int] -> Bool
isTriangle [a, b, c] = (a + b > c) && (a + c> b) && (b + c > a)
isTriangle _ = False

parseProb1, parseProb2 :: String -> [[Int]]
parseProb1 input = map (map read . words) (lines input)
parseProb2 input =
    let (x, y, z) = unzip3 $ map (toTriple . map read . words) (lines input)
    in chunksOf 3 $ concat [x, y, z]

solverProb1, solverProb2 :: String -> String
solverProb1 input = show $ length $ filter id $ map isTriangle (parseProb1 input)
solverProb2 input = show $ length $ filter id $ map isTriangle (parseProb2 input)

toTriple :: [a] -> (a, a, a)
toTriple [a, b, c] = (a, b, c)
toTriple _ = error "Only work with list of length 3"