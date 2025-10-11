import Data.Char (intToDigit)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Map (Map)
import qualified Data.Map as Map
import Numeric (showIntAtBase)

type Coords = (Int, Int)
type Steps = Int
type Candidates = Seq (Coords, Steps)
type Visited = Map Coords Steps

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
  let s = MazeState {candidates = Seq.singleton (start, 0), visited = Map.empty}
   in exploreMaze s end

exploreMaze :: MazeState -> Coords -> Int
exploreMaze state target =
  let ((currentCoords, currentSteps) Seq.:<| candidates') = candidates state
      nextCandidates = generateCandidates (currentCoords, currentSteps) $ visited state
      state' = MazeState {candidates = candidates' Seq.>< Seq.fromList nextCandidates, visited = Map.insert currentCoords currentSteps $ visited state}
   in if currentCoords == target
        then currentSteps + 1
        else exploreMaze state' target

generateCandidates :: (Coords, Steps) -> Visited -> [(Coords, Steps) ]
generateCandidates ((x, y), steps) v = [
  (c, s) |
    (dx, dy) <- [(0, 1), (0, -1), (-1, 0), (1, 0)],
    let c = (x + dx, y + dy),
    let s = steps + 1,
    isOpenSpace c,
    c `Map.notMember` v,
    fst c >= 0,
    snd c >= 0]

isOpenSpace :: Coords -> Bool
isOpenSpace (x, y) =
  let sumNum = x * x + 3 * x + 2 * x * y + y + y * y + favNum
      binString = showIntAtBase 2 intToDigit sumNum ""
   in even $ length $ filter (== '1') binString
