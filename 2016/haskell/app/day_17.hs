import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

problemFilename :: String
problemFilename = "../inputs/day_17.txt"

data Direction = U | D | L | R deriving (Enum, Show)
type Coords = (Int, Int)
type Candidates = Seq (Coords, String)

startCoords, endCoords :: Coords
startCoords = (0, 0)
endCoords = (3, 3)

main :: IO ()
main = do
  passcode <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 passcode
  putStrLn $ "Problem 2: " ++ solverProb2 passcode

solverProb1, solverProb2 :: String -> String
solverProb1 initPass = extractPath initPass $ searchVault $ Seq.singleton (startCoords, initPass)
solverProb2 = undefined

-- ne pas oublier de filtrer ce qui est hors grille
searchVault :: Candidates -> String
searchVault candidates =
  let ((currentCoords, currentPass) Seq.:<| candidates') = candidates
      nextDirections = openDirections $ computeHash currentPass
      nextCandidates = [(c, p) | d <- nextDirections, let c = move d currentCoords, let p = currentPass ++ show d, validCoords c]
   in if currentCoords == endCoords
        then currentPass
        else searchVault $ candidates' Seq.>< Seq.fromList nextCandidates

move :: Direction -> Coords -> Coords
move U (x, y) = (x, y - 1)
move D (x, y) = (x, y + 1)
move L (x, y) = (x - 1, y)
move R (x, y) = (x + 1, y)

validCoords :: Coords -> Bool
validCoords (x, y) = all (`elem` [0 .. 3]) [x, y]

extractPath :: String -> String -> String
extractPath initPass = drop (length initPass)

openDirections :: String -> [Direction]
openDirections pass = [toEnum idx | idx <- [0 .. 3], pass !! idx `elem` ['b' .. 'f']]

computeHash :: String -> String
computeHash = BS.unpack . B16.encode . MD5.hash . BS.pack