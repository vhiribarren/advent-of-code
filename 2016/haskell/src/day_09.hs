problemFilename :: String
problemFilename = "../inputs/day_09.txt"

main :: IO ()
main = do
  input <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 input
  putStrLn $ "Problem 2: " ++ solverProb2 input

solverProb1, solverProb2 :: String -> String
solverProb1 = show . length . expandCompressed
solverProb2 = show . fullExpandedLength

parseExpansionCommand :: String -> (Int, Int)
parseExpansionCommand command =
  let (left, right) = break (== 'x') command
   in (read left, read $ tail right)

expandCompressed :: String -> String
expandCompressed compressed = snd $ continueExpansion (compressed, [])

continueExpansion :: (String, String) -> (String, String)
continueExpansion ([], expanded) = ([], expanded)
continueExpansion (compressed, expanded) = case head compressed of
  '(' ->
    let (left, right) = break (== ')') $ tail compressed
        (size, times) = parseExpansionCommand left
        chunk = take size (drop 1 right)
        nextCompressed = drop (1 + size) right
     in continueExpansion (nextCompressed, expanded ++ concat (replicate times chunk))
  c -> continueExpansion (tail compressed, expanded ++ [c])

fullExpandedLength :: String -> Int
fullExpandedLength input = snd $ continueFullExpansion (input, 0)

continueFullExpansion :: (String, Int) -> (String, Int)
continueFullExpansion ([], len) = ([], len)
continueFullExpansion (compressed, len) = case head compressed of
  '(' ->
    let (left, right) = break (== ')') $ tail compressed
        (size, times) = parseExpansionCommand left
        chunk = take size (drop 1 right)
        nextCompressed = drop (1 + size) right
        nextLen = times * snd (continueFullExpansion (chunk, 0))
     in continueFullExpansion (nextCompressed, len + nextLen)
  _ -> continueFullExpansion (tail compressed, len + 1)