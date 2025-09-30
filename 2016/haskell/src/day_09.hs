problemFilename :: String
problemFilename = "../inputs/day_09.txt"

main :: IO ()
main = do
  input <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 input
  putStrLn $ "Problem 2: " ++ solverProb2 input

solverProb1, solverProb2 :: String -> String
solverProb1 = show . fst . continueSimpleExpansion 0
solverProb2 = show . fst . continueFullExpansion 0

continueSimpleExpansion :: Int -> String -> (Int, String)
continueSimpleExpansion len [] = (len, [])
continueSimpleExpansion len compressed = case head compressed of
  '(' ->
    let (left, right) = break (== ')') $ tail compressed
        (size, times) = parseExpansionCommand left
        nextCompressed = drop (1 + size) right
     in continueSimpleExpansion (len + times * size) nextCompressed
  _ -> continueSimpleExpansion (len + 1) (tail compressed)

continueFullExpansion :: Int -> String -> (Int, String)
continueFullExpansion len [] = (len, [])
continueFullExpansion len compressed = case head compressed of
  '(' ->
    let (left, right) = break (== ')') $ tail compressed
        (size, times) = parseExpansionCommand left
        chunk = take size (drop 1 right)
        nextCompressed = drop (1 + size) right
        nextLen = times * fst (continueFullExpansion 0 chunk)
     in continueFullExpansion (len + nextLen) nextCompressed
  _ -> continueFullExpansion (len + 1) (tail compressed)

parseExpansionCommand :: String -> (Int, Int)
parseExpansionCommand command =
  let (left, right) = break (== 'x') command
   in (read left, read $ tail right)