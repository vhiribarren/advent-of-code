import Text.Megaparsec
import Text.Megaparsec.Char (string, letterChar, space1, eol, char)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void (Void)
import Control.Monad (void)


problemFilename :: String
problemFilename = "../inputs/day_22.txt"

type Parser a = Parsec Void String a

data Node = Node {
    nodeId :: String,
    nodeX :: Int,
    nodeY :: Int,
    nodeSize :: Int,
    nodeUsed :: Int,
    nodeAvail :: Int,
    nodeUsePercent :: Int
} deriving (Show, Eq)

main :: IO ()
main = do
  input <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 input
  putStrLn $ "Problem 2: " ++ solverProb2 input

solverProb1, solverProb2 :: String -> String
solverProb1 input = case parse parseNodes "" (unlines $ drop 2 $ lines input) of
  Left err -> error (errorBundlePretty err)
  Right nodes -> show $ length $ viableNodes nodes
solverProb2 input = undefined

parseNodes :: Parser [Node]
parseNodes = parseNode `sepEndBy1` eol

parseNode :: Parser Node
parseNode = do
    (name, (x, y)) <- parseFilename <* space1
    size <- decimal <* char 'T' <* space1
    used <- decimal <* char 'T' <* space1
    avail <- decimal <* char 'T' <* space1
    usePercent <- decimal <* char '%'
    return $ Node name x y size used avail usePercent
  where
    parseFilename :: Parser (String, (Int, Int))
    parseFilename = match $ do
      void $ string "/dev/grid/node-x"
      x <- decimal
      void $ string "-y"
      y <- decimal
      return (x, y)

viableNodes :: [Node] -> [(Node, Node)]
viableNodes nodes = [(a, b) |
    a <- nodes,
    b <- nodes,
    a /= b,
    nodeUsed a > 0,
    nodeUsed a <= nodeAvail b
  ]