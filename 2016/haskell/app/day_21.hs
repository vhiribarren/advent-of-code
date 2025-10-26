import Text.Megaparsec
import Data.List (elemIndex)
import Text.Megaparsec.Char (string, letterChar, space1, eol, char)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void (Void)


problemFilename :: String
problemFilename = "../inputs/day_21.txt"

initPassword :: String
initPassword = "abcdefgh"

type Parser a = Parsec Void String a

data Instruction =
  SwapPosition Int Int |
  SwapLetter Char Char |
  RotateLeft Int |
  RotateRight Int |
  RotatePosition Char |
  Reverse Int Int |
  Move Int Int
  deriving (Show)

main :: IO ()
main = do
  input <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 input
  putStrLn $ "Problem 2: " ++ solverProb2 input

solverProb1, solverProb2 :: String -> String
solverProb1 input = case parse parseInstructions "" input of
  Left err -> error (errorBundlePretty err)
  Right instructions -> foldl applyInstruction initPassword instructions
solverProb2 input = undefined

parseInstructions :: Parser [Instruction]
parseInstructions = parseInstruction `sepEndBy1` eol

parseInstruction :: Parser Instruction
parseInstruction = choice [parseSwapPosition, parseSwapLetter, parseRotateLeft, parseRotateRight, parseRotationPosition, parseReverse, parseMove ]
  where
    parseSwapPosition = SwapPosition
      <$> (string "swap position" *> space1 *>  decimal)
      <*> (space1 *> string "with position" *> space1 *>  decimal)
    parseSwapLetter = SwapLetter
      <$> (string "swap letter" *> space1 *>  letterChar)
      <*> (space1 *> string "with letter" *> space1 *> letterChar)
    parseRotateLeft = RotateLeft
      <$> (string "rotate left" *> space1 *>  decimal)
      <*  space1 <* string "step" <*  optional (char 's')
    parseRotateRight = RotateRight
      <$> (string "rotate right" *> space1 *>  decimal)
      <*  space1 <* string "step" <*  optional (char 's')
    parseRotationPosition = RotatePosition
      <$> (string "rotate based on position of letter" *> space1 *> letterChar)
    parseReverse = Reverse
      <$> (string "reverse positions" *> space1 *>  decimal)
      <*> (space1 *> string "through" *> space1 *> decimal)
    parseMove = Move
      <$> (string "move position" *> space1 *> decimal)
      <*> (space1 *> string "to position" *> space1 *> decimal)

applyInstruction :: String -> Instruction-> String
applyInstruction input instr = case instr of
  SwapPosition x y ->
    replaceNth y (input !! x) . replaceNth x (input !! y) $ input 
  SwapLetter x y ->
    fmap (\c -> if c == x then y else if c == y then x else c) input 
  RotateLeft x ->
    let (l, r) = splitAt x input in r ++ l
  RotateRight x ->
    let (l, r) = splitAt  (length input - x) input in r ++ l
  RotatePosition x ->
    let Just idx = elemIndex x input
        rotation = (idx + 1 + (if idx >= 4 then 1 else 0)) `mod` length input
    in applyInstruction input (RotateRight rotation)
  Reverse x y ->
    take x input ++ reverse (take (y - x +1) $ drop x input)  ++ drop (y+1) input
  Move x y ->
    let letter = input !! x
        (l, r) = splitAt x input
        (withRemovedLeft, withRemovedRight) = splitAt y $ l ++ tail r
    in withRemovedLeft ++ [letter] ++ withRemovedRight

replaceNth :: Int -> Char -> String -> String
replaceNth n newChar str =
  take n str ++ [newChar] ++ drop (n + 1) str