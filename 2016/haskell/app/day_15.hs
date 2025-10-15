import Data.List (groupBy)
import Data.Char (isDigit)

problemFilename :: String
problemFilename = "../inputs/day_15.txt"

type Time = Int

data Disc = Disc {
  discId :: Int,
  positions :: Int,
  current_time :: Int,
  current_pos :: Int
} deriving (Show)

main :: IO ()
main = do
  input <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 input
  -- putStrLn $ "Problem 2: " ++ solverProb2 input

solverProb1, solverProb2 :: String -> String
solverProb1 = show . searchStartTime . inputParser
solverProb2 = undefined

inputParser :: String -> [Disc]
inputParser input = map parseLine (lines input)
  where parseLine line = case fmap read $ filter (all isDigit) $ groupBy (\l r -> isDigit l == isDigit r) line of
          [a, b, c, d] -> Disc a b c d
          _ -> error "parsing error"

searchStartTime :: [Disc] -> Time
searchStartTime = go 0 where
  go t d = if all (isDiscValidPosition t) d
    then t
    else go (t+1) d

isDiscValidPosition :: Time -> Disc -> Bool
isDiscValidPosition t d  = 0 == (t + discId d + current_pos d) `mod` positions d