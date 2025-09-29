{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Data.List (isInfixOf)

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
  putStrLn $ "Problem 2: " ++ solverProb2 input

solverProb1, solverProb2 :: String -> String
solverProb1 = show . length . filter hasAddressTLS . parse
solverProb2 = show . length . filter hasAddressSSL . parse

parse :: String -> [IPV7Address]
parse = map (parseAddress [] [] False) . lines

parseAddress :: [String] -> [String] -> Bool -> String -> IPV7Address
parseAddress h o _ [] = IPV7Address {hypernets = h, others = o}
parseAddress h o False input =
  let (newOther, next) = break (== '[') input
   in parseAddress h (newOther : o) True (tailGard next)
parseAddress h o True input =
  let (newHypernet, next) = break (== ']') input
   in parseAddress (newHypernet : h) o False (tailGard next)

tailGard :: [a] -> [a]
tailGard xs
  | null xs = []
  | otherwise = tail xs

hasAddressTLS :: IPV7Address -> Bool
hasAddressTLS (IPV7Address h o) = any hasABBA o && not (any hasABBA h)

hasABBA :: String -> Bool
hasABBA s@(a : b : c : d : _)
  | a == b = hasABBA (tail s)
  | (a == d) && (b == c) = True
  | otherwise = hasABBA (tail s)
hasABBA _ = False

hasAddressSSL :: IPV7Address -> Bool
hasAddressSSL ip@(IPV7Address hypernets' _) =
  let allBAB = map convertABA2BAB (findAllABA ip)
      hasHypernetBAB hypernet = any (`isInfixOf` hypernet) allBAB
   in any hasHypernetBAB hypernets'

findAllABA :: IPV7Address -> [String]
findAllABA (IPV7Address _ others') =
  let searchABA results [] = results
      searchABA results string =
        if isABA string
          then searchABA (take 3 string : results) (tail string)
          else searchABA results (tail string)
   in concatMap (searchABA []) others'

isABA :: String -> Bool
isABA [a, b, c]
  | a == b = False
  | a == c = True
  | otherwise = False
isABA _ = False

convertABA2BAB :: String -> String
convertABA2BAB [a, b, _] = [b, a, b]
convertABA2BAB _ = error "Invalided ABA"
