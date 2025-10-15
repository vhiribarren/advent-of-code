import Data.List.Split (chunksOf)
problemFilename :: String
problemFilename = "../inputs/day_16.txt"

firstDiskSize :: Int
firstDiskSize = 272

main :: IO ()
main = do
  input <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 input
  putStrLn $ "Problem 2: " ++ solverProb2 input

solverProb1, solverProb2 :: String -> String
solverProb1 = checksum . fullDataExpansion firstDiskSize
solverProb2 = undefined

expandData :: String -> String
expandData a = a ++ "0" ++ b
  where b =  map (\c -> if c == '1' then '0' else '1') $ reverse a

fullDataExpansion :: Int -> String -> String
fullDataExpansion size state = if length state >= size
  then take size state
  else fullDataExpansion size $ expandData state

checksum :: String -> String
checksum input =
  let c = map convertPair $ chunksOf 2 input
   in if even (length c) then checksum c else c

convertPair :: String -> Char
convertPair "00" = '1'
convertPair "11" = '1'
convertPair _ = '0'