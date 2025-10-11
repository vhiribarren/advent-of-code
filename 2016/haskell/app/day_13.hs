import Data.Char (intToDigit)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Map (Map)
import qualified Data.Map as Map
import Numeric (showIntAtBase)

type Coords = (Int, Int)
type CurrentCandidate = Coords
type PrecCandidate = Maybe Coords
type Candidates = Seq (CurrentCandidate, PrecCandidate)
type Visited = Map CurrentCandidate PrecCandidate

data MazeState = MazeState
  { candidates :: Candidates,
    visited :: Visited
  }

favNum :: Int
favNum = 1350

initCoords, targetCoords :: Coords
initCoords = (1, 1)
targetCoords = (31, 39)

main :: IO ()
main = do
  putStrLn $ "Problem 1: " ++ solverProb1
-- putStrLn $ "Problem 2: " ++ solverProb2 input

solverProb1, solverProb2 :: String
solverProb1 = show $ findTarget initCoords targetCoords
solverProb2 = undefined

findTarget :: Coords -> Coords -> Int
findTarget start end =
  let s = MazeState {candidates = Seq.singleton (start, Nothing), visited = Map.empty}
   in exploreMaze s end

exploreMaze :: MazeState -> Coords -> Int
exploreMaze state target =
  let ((currentCoords, prec) Seq.:<| candidates') = candidates state
      nextCandidates = generateCandidates currentCoords $ visited state
      state' = MazeState {candidates = candidates' Seq.>< Seq.fromList nextCandidates, visited = Map.insert currentCoords prec $ visited state}
   in if currentCoords == target
        then chainLength currentCoords $ visited state'
        else exploreMaze state' target

generateCandidates :: Coords -> Visited -> [(CurrentCandidate, PrecCandidate)]
generateCandidates (x, y) v = [
  (c, Just (x, y)) |
    (dx, dy) <- [(0, 1), (0, -1), (-1, 0), (1, 0)],
    let c = (x + dx, y + dy),
    isOpenSpace c,
    c `Map.notMember` v,
    fst c >= 0,
    snd c >= 0]

chainLength :: Coords -> Visited -> Int
chainLength c v = case Map.lookup c v of
  Nothing -> 0
  Just Nothing -> 0
  Just (Just p) -> 1 + chainLength p v

isOpenSpace :: Coords -> Bool
isOpenSpace (x, y) =
  let sumNum = x * x + 3 * x + 2 * x * y + y + y * y + favNum
      binString = showIntAtBase 2 intToDigit sumNum ""
   in even $ length $ filter (== '1') binString
