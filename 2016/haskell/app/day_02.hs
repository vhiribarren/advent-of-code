import Data.Maybe (isJust, fromJust)
problemFilename :: String
problemFilename = "../inputs/day_02.txt"

type Coord = (Int, Int)
type CoordDelta = Coord
type KeyPad = (Coord -> Maybe Char)

main :: IO ()
main = do
  input <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 input
  putStrLn $ "Problem 2: " ++ solverProb2 input

parse :: String -> [String]
parse = lines

solverProb1, solverProb2 :: String -> String
solverProb1 = displayCode fstKeypad . codeNumbers fstKeypad fstInitCoord . parse
solverProb2 = displayCode sndKeypad . codeNumbers sndKeypad sndInitCoord . parse

fstInitCoord, sndInitCoord :: Coord
fstInitCoord = (1, 1)
sndInitCoord = (0, 2)

fstKeypad, sndKeypad :: Coord -> Maybe Char
fstKeypad c = case c of
    (0, 0) -> Just '1'
    (1, 0) -> Just '2'
    (2, 0) -> Just '3'
    (0, 1) -> Just '4'
    (1, 1) -> Just '5'
    (2, 1) -> Just '6'
    (0, 2) -> Just '7'
    (1, 2) -> Just '8'
    (2, 2) -> Just '9'
    _ -> Nothing
sndKeypad c = case c of
    (2, 0) -> Just '1'
    (1, 1) -> Just '2'
    (2, 1) -> Just '3'
    (3, 1) -> Just '4'
    (0, 2) -> Just '5'
    (1, 2) -> Just '6'
    (2, 2) -> Just '7'
    (3, 2) -> Just '8'
    (4, 2) -> Just '9'
    (1, 3) -> Just 'A'
    (2, 3) -> Just 'B'
    (3, 3) -> Just 'C'
    (2, 4) -> Just 'D'
    _ -> Nothing

convertInst2Delta :: Char -> CoordDelta
convertInst2Delta c = case c of
  'U' -> (0, -1)
  'D' -> (0, 1)
  'L' -> (-1, 0)
  'R' -> (1, 0)
  other -> error $ "Invalid instruction: " ++ [other]

nextCoord :: KeyPad  -> Coord -> [Char] -> Coord
nextCoord keyPad startCoord instructions =
  let nextStep (x, y) instruction =
        let (dx, dy) = convertInst2Delta instruction
            (cx, cy) = (x+dx, y+dy)
            isInKeypad = keyPad (cx, cy)
        in if isJust isInKeypad then (cx, cy) else (x, y)
   in foldl nextStep startCoord instructions

codeNumbers :: KeyPad -> Coord -> [String] -> [Coord]
codeNumbers keyPad startCoord (inst:nextInsts) =
    let coord =  nextCoord keyPad startCoord inst
    in coord : codeNumbers keyPad coord nextInsts
codeNumbers _ _ [] = []

displayCode ::  KeyPad -> [Coord] -> String
displayCode keyPad = map (fromJust . keyPad)