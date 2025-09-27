import qualified Data.ByteString.Char8 as B
import qualified Crypto.Hash.MD5       as MD5
import           Data.ByteString.Base16 (encode)
import Data.List (isPrefixOf)

problemFilename :: String
problemFilename = "../inputs/day_05.txt"

main :: IO ()
main = do
  input <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 input
  -- putStrLn $ "Problem 2: " ++ solverProb2 input

solverProb1 :: String -> String
solverProb1 = computePassword [] 0

computePassword :: [Char] -> Int -> String -> String
computePassword password index doorId =
  if length password == 8
  then reverse password
  else let (newChar, newIndex) = nextCodeChar index doorId
       in computePassword (newChar : password) newIndex doorId

nextCodeChar ::  Int -> String -> (Char, Int)
nextCodeChar index doorId =
  let hash = computeMD5 $ doorId ++ show index
  in if "00000" `isPrefixOf` hash
     then (hash !! 5, index +1)
     else nextCodeChar (index+1) doorId

computeMD5 :: String -> String
computeMD5 = B.unpack . encode . MD5.hash . B.pack

