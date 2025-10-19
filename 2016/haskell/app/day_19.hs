import Data.Sequence (Seq ((:<|)), (|>))
import qualified Data.Sequence as Seq

elvesCount :: Int
elvesCount = 3004953

main :: IO ()
main = do
  putStrLn $ "Problem 1: " ++ solverProb1
  putStrLn $ "Problem 2: " ++ solverProb2

solverProb1, solverProb2 :: String
solverProb1 = show $ playSantaRound1 $ Seq.fromList [1..elvesCount]
solverProb2 = show $ playSantaRound2 $ Seq.fromList [1..elvesCount]

playSantaRound1 :: Seq Int -> Int
playSantaRound1 (elf1 :<| _ :<| otherElves) = playSantaRound1 (otherElves |> elf1)
playSantaRound1 (elf :<| Seq.Empty) = elf
playSantaRound1 _ = error "should not happen"

playSantaRound2 :: Seq Int -> Int
playSantaRound2 (elf :<| Seq.Empty) = elf
playSantaRound2 elves@(elf :<| otherElves) =
  let removeIdx = (Seq.length elves `div` 2) - 1
   in playSantaRound2 (Seq.deleteAt removeIdx otherElves |> elf)
playSantaRound2 _ = error "should not happen"