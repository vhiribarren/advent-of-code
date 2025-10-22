import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List.Split
import Control.Monad.State

problemFilename :: String
problemFilename = "../inputs/day_20.txt"

data BlacklistDirection = L | R deriving (Show, Eq)
type RangeBorder = (Int, BlacklistDirection)
type Ranges = IntMap BlacklistDirection

maxAddress :: Int
maxAddress = 4294967295

main :: IO ()
main = do
  input <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 input
  putStrLn $ "Problem 2: " ++ solverProb2 input

solverProb1, solverProb2 :: String -> String
solverProb1 input = show $ findMin $ (-1, L) : IntMap.toList (mergeInput input)
solverProb2 input = show $ countAddresses $ (-1, L) : IntMap.toList (mergeInput input)

mergeInput :: String -> Ranges
mergeInput input = foldr (fillRanges . map read . splitOn "-") IntMap.empty (lines input)

fillRanges :: [Int] -> Ranges -> Ranges
fillRanges [left, right] m = flip execState m $ do
    let beforeLeft = IntMap.lookupLT left m
        afterRight = IntMap.lookupGT right m
    case beforeLeft of
      Just(_, R) -> return ()
      _ -> modify (IntMap.insert left R)
    case afterRight of
      Just(_, L) -> return ()
      _ -> modify (IntMap.insert right L)
    s <- get
    let (initRange,_, startRange) = IntMap.splitLookup (left+1) s
        (_, _, endRange) = IntMap.splitLookup (right-1) startRange
    put (IntMap.union initRange endRange)
fillRanges _ _ = error "fillRanges - should not happen"

findMin :: [RangeBorder] -> Int
findMin ranges@((l, L):(r, R):_) = if l /= r && l+1 /= r
  then l+1
  else findMin $ tail ranges
findMin ranges@(_:_:_) = findMin $ tail ranges
findMin _ = error "findMin - should not happen"

countAddresses :: [RangeBorder] -> Int
countAddresses ranges =
  let couples = filter (\((_, rdir), _) -> rdir == R) $ zip ranges (tail ranges)
   in 1 + maxAddress - foldl (\acc ((l, _), (r, _)) -> acc + r - l + 1) 0 couples