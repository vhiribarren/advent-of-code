import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Char8 as B
import Data.List (isPrefixOf, sortOn)
import Data.Maybe (isJust)
import Text.Read (readMaybe)

problemFilename :: String
problemFilename = "../inputs/day_05.txt"

main :: IO ()
main = do
  input <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 input
  putStrLn $ "Problem 2: " ++ solverProb2 input

solverProb1, solverProb2 :: String -> String
solverProb1 = computePasswordProb1 [] 0
solverProb2 = computePasswordProb2 [] 0

computePasswordProb1 :: [Char] -> Int -> String -> String
computePasswordProb1 password index doorId =
  if length password == 8
    then reverse password
    else
      let (newIndex, newChar, _) = nextCodeChar index doorId
       in computePasswordProb1 (newChar : password) newIndex doorId

computePasswordProb2 :: [(Int, Char)] -> Int -> String -> String
computePasswordProb2 password index doorId =
  if length password == 8
    then snd <$> sortOn fst password
    else
      let (newIndex, charPosStr, newChar) = nextCodeChar index doorId
          charPos = readMaybe [charPosStr] :: Maybe Int
          newPassword = case charPos of
            Nothing -> password
            Just pos -> if isJust (lookup pos password) || pos > 7 then password else (pos, newChar) : password
       in computePasswordProb2 newPassword newIndex doorId

nextCodeChar :: Int -> String -> (Int, Char, Char)
nextCodeChar index doorId =
  let hash = computeMD5 $ doorId ++ show index
   in if "00000" `isPrefixOf` hash
        then (index + 1, hash !! 5, hash !! 6)
        else nextCodeChar (index + 1) doorId

computeMD5 :: String -> String
computeMD5 = B.unpack . encode . MD5.hash . B.pack