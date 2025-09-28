{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

problemFilename :: String
problemFilename = "../inputs/day_07.txt"

data IPV7Address = IPV7Address
  { hypernets :: [String],
    others :: [String]
  }
  deriving (Show)

main :: IO ()
main = do
  input <- readFile problemFilename
  putStrLn $ "Problem 1: " ++ solverProb1 input

-- putStrLn $ "Problem 2: " ++ solverProb2 input

solverProb1 :: String -> String
solverProb1 = show . length . filter isAddressValid . parse

parse :: String -> [IPV7Address]
parse = map (extract [] [] False) . lines

extract :: [String] -> [String] -> Bool -> String -> IPV7Address
extract h o _ [] = IPV7Address {hypernets = h, others = o}
extract h o False input =
  let (newOther, next) = break (== '[') input
   in extract h (newOther : o) True (tailGard next)
extract h o True input =
  let (newHypernet, next) = break (== ']') input
   in extract (newHypernet : h) o False (tailGard next)

tailGard :: [a] -> [a]
tailGard xs
  | null xs = []
  | otherwise = tail xs

isAddressValid :: IPV7Address -> Bool
isAddressValid (IPV7Address h o) = any hasABBA o && not (any hasABBA h)

hasABBA :: String -> Bool
hasABBA s@(a:b:c:d:_)
  | a == b = hasABBA (tail s)
  | (a == d) && (b == c) = True
  | otherwise = hasABBA (tail s)
hasABBA _ = False