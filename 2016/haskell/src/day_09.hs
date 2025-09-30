problemFilename :: String
problemFilename = "../inputs/day_09.txt"

main :: IO ()
main = do
  input <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 input

solverProb1 :: String -> String
solverProb1 = show . length . expandCompressed

expandCompressed :: String -> String
expandCompressed compressed = snd $ continueExpansion (compressed, [])

continueExpansion :: (String, String) -> (String, String)
continueExpansion ([], expanded) = ([], expanded)
continueExpansion (compressed, expanded) = case head compressed of
  '(' -> let (left, right) = break (== ')') $ tail compressed
             (size, times) = parseExpansionCommand left
             chunk = take size (drop 1 right)
             nextCompressed = drop (1+size) right
          in continueExpansion (nextCompressed, expanded ++ concat (replicate times chunk))
  c -> continueExpansion (tail compressed, expanded ++ [c])

parseExpansionCommand :: String -> (Int, Int)
parseExpansionCommand command =
  let (left, right) = break (=='x') command
   in (read left, read $ tail right)