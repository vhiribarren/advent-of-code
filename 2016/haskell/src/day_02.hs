import Data.Char (intToDigit)
problemFilename :: String
problemFilename = "../inputs/day_02.txt"

type Coord = (Int, Int)
type CoordDelta = Coord

main :: IO ()
main = do
  input <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 input

parse :: String -> [String]
parse = lines

solverProb1 :: String -> String
solverProb1 input = displayCode $ codeNumbers initCoord $ parse input

initCoord :: Coord
initCoord = (1, 1)

convertInst2Delta :: Char -> CoordDelta
convertInst2Delta c = case c of
  'U' -> (0, -1)
  'D' -> (0, 1)
  'L' -> (-1, 0)
  'R' -> (1, 0)
  _ -> undefined

coord2key :: Coord -> Int
coord2key (x, y) = 1 + x + 3 * y

clamp :: Int -> Int -> Int -> Int
clamp lo hi x = max lo (min hi x)

nextCoord :: Coord -> [Char] -> Coord
nextCoord startCoord instructions =
  let nextStep (x, y) instruction =
        let (dx, dy) = convertInst2Delta instruction
         in (clamp 0 2 $ x + dx, clamp 0 2 $ y + dy)
   in foldl nextStep startCoord instructions

codeNumbers :: Coord -> [String] -> [Coord]
codeNumbers startCoord (inst:nextInsts) =
    let coord =  nextCoord startCoord inst
    in coord : codeNumbers coord nextInsts
codeNumbers _ [] = []

displayCode :: [Coord] -> String
displayCode = map (intToDigit . coord2key)