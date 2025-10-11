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

maxSteps :: Int
maxSteps = 50

main :: IO ()
main = do
  putStrLn $ "Problem 1: " ++ solverProb1
  putStrLn $ "Problem 2: " ++ solverProb2

solverProb1, solverProb2 :: String
solverProb1 = show $ findTarget initCoords targetCoords
solverProb2 = show $ findLocations initCoords maxSteps

findTarget :: Coords -> Coords -> Int
findTarget start end =
  let s = MazeState {candidates = Seq.singleton (start, 0), visited = Map.empty}
   in exploreMazeUntilCoords s end

findLocations :: Coords -> Int -> Int
findLocations start maxSteps' =
  let s = MazeState {candidates = Seq.singleton (start, 0), visited = Map.empty}
   in length $ exploreMazeUntilSteps s maxSteps'

exploreMazeUntilCoords :: MazeState -> Coords -> Int
exploreMazeUntilCoords state target =
  let ((currentCoords, currentSteps) Seq.:<| candidates') = candidates state
      nextCandidates = generateCandidates (currentCoords, currentSteps) $ visited state
      state' = MazeState {candidates = candidates' Seq.>< Seq.fromList nextCandidates, visited = Map.insert currentCoords currentSteps $ visited state}
   in if currentCoords == target
        then currentSteps + 1
        else exploreMazeUntilCoords state' target

exploreMazeUntilSteps :: MazeState -> Int -> Visited
exploreMazeUntilSteps state maxSteps =
  let ((currentCoords, currentSteps) Seq.:<| candidates') = candidates state
      nextCandidates = generateCandidates (currentCoords, currentSteps) $ visited state
      state' = MazeState {candidates = candidates' Seq.>< Seq.fromList nextCandidates, visited = Map.insert currentCoords currentSteps $ visited state}
   in if currentSteps > maxSteps
        then visited state
        else exploreMazeUntilSteps state' maxSteps

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
