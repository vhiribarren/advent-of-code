import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS
import Data.List (group, find)
import Data.Maybe (isJust)

type Hashes = [(Int, String)]

salt :: String
salt = "abc"

main :: IO ()
main = do
  putStrLn $ "Problem 1: " ++ solverProb1
  putStrLn $ "Problem 2: " ++ solverProb2

solverProb1, solverProb2 :: String
solverProb1 = show $ validHashIndexes hashes  !! 63
solverProb2 = show $ validHashIndexes strechedHashes  !! 63

validHashIndexes :: Hashes -> [Int]
validHashIndexes h =
  if checkHead h
    then fst (head h):validHashIndexes (drop 1 h)
    else validHashIndexes (drop 1 h)

checkHead ::  Hashes -> Bool
checkHead ((_,x):xs) = case hasTriple x of
  Nothing -> False
  Just c -> isJust $ find (\(_,v) -> hasQuintuple v == Just c) (take 1000 xs)
checkHead [] = error "Should not happen"

hasTriple, hasQuintuple :: String -> Maybe Char
hasTriple = hasMultiple 3
hasQuintuple = hasMultiple 5

hasMultiple:: Int -> String -> Maybe Char
hasMultiple n hash = head <$> find (\v -> length v >= n) (group hash)

hashes, strechedHashes :: Hashes
hashes = zip [0..] $ fmap (\idx -> computeHash $ salt ++ show (idx::Int)) [0..]
strechedHashes = zip [0..] $ fmap (\idx -> iterate computeHash (salt ++ show (idx::Int)) !! 2017) [0..]

computeHash :: String -> String
computeHash = BS.unpack . B16.encode . MD5.hash . BS.pack