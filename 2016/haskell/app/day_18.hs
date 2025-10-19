problemFilename :: String
problemFilename = "../inputs/day_18.txt"

data Tile = Safe | Trap deriving (Eq)

prob1TotalRows, prob2TotalRows  :: Int
prob1TotalRows = 40
prob2TotalRows = 400000

main :: IO ()
main = do
  passcode <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 passcode
  putStrLn $ "Problem 2: " ++ solverProb2 passcode

solverProb1, solverProb2 :: String -> String
solverProb1 input = show $ countSafe $  take prob1TotalRows $ iterate nextRow (parseInput input)
solverProb2 input = show $ countSafe $  take prob2TotalRows $ iterate nextRow (parseInput input)

parseInput :: String -> [Tile]
parseInput = fmap convertChar
  where convertChar '.' = Safe
        convertChar '^' = Trap
        convertChar _ = error "should not happen"

countSafe :: [[Tile]] -> Int
countSafe tileRows = length $ concatMap (filter (== Safe)) tileRows

getTile :: [Tile] -> Int -> Tile
getTile tiles idx
  | idx < 0 = Safe
  | idx >= length tiles = Safe
  | otherwise = tiles !! idx

nextRow :: [Tile] -> [Tile]
nextRow precRow = fmap computeNextRow [0..length precRow -1] where
  computeNextRow idx = case getTile precRow . (+idx) <$> [-1, 0, 1] of
    [Trap, Trap, Safe] -> Trap
    [Safe, Trap, Trap] -> Trap
    [Safe, Safe, Trap] -> Trap
    [Trap, Safe, Safe] -> Trap
    _ -> Safe