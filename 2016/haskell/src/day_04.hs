import Data.List (group, sort, sortBy)
import Text.Regex.TDFA

problemFilename :: String
problemFilename = "../inputs/day_04.txt"

data Room = Room
  { rawId :: String,
    encryptedName :: String,
    sectorId :: Int,
    checksum :: String
  }
  deriving (Show)

main :: IO ()
main = do
  input <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 input

rawIdToRoom :: String -> Room
rawIdToRoom input =
  let regex = "^(.*)-([0-9]+)\\[(.*)\\]" :: String -- Does not recognize '\d'...
      groups = getAllTextSubmatches (input =~ regex :: AllTextSubmatches [] String)
   in case groups of
        [_, encryptedNameG, sectorIdG, checksumG] -> Room input encryptedNameG (read sectorIdG) checksumG
        other -> error $ show other

isValidRoom :: Room -> Bool
isValidRoom room = computeChecksum room == checksum room

computeChecksum :: Room -> String
computeChecksum room = take 5 $ map head $ sortBy (\l r -> length r `compare` length l) $ group . sort $ filter (/= '-') (encryptedName room)

parse :: String -> [Room]
parse input = map rawIdToRoom (lines input)

solverProb1 :: String -> String
solverProb1 input = show $ sum [sectorId room | room <- parse input, isValidRoom room]
