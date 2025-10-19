import Data.Sequence (Seq ((:<|)), (|>))
import qualified Data.Sequence as Seq

elvesCount :: Int
elvesCount = 3004953

main :: IO ()
main = do
  putStrLn $ "Problem 1: " ++ solverProb1
  putStrLn $ "Problem 2: " ++ solverProb2

solverProb1, solverProb2 :: String
solverProb1 = show $ playSantaGame $ Seq.fromList [1..elvesCount]
solverProb2 = undefined

playSantaGame :: Seq Int -> Int
playSantaGame (elf1 :<| _ :<| otherElves) = playSantaGame (otherElves |> elf1)
playSantaGame (elf :<| Seq.Empty) = elf
playSantaGame _ = error "should not happen"
