#!/usr/bin/env cabal

{- cabal:
build-depends:
  base ^>=4.17.0.0,
  filepath ^>= 1.4.2.2,
-}

{-# LANGUAGE CPP #-}

import System.FilePath
import Data.List (sort)
import Data.Bifunctor (bimap)

inputFile :: FilePath
inputFile = ".." </> "inputs" </> takeFileName __FILE__ -<.> ".txt"

parseInput :: String -> ([Int], [Int])
parseInput input = process (lines input) [] [] where
    process [] left right = (left, right)
    process (x:xs) left right =
      let [l, r] = map read $ words x
      in process xs (l:left) (r:right)

distances :: ([Int], [Int]) -> [Int]
distances (left, right) = zipWith (\x y -> abs (x - y)) left right

similarities :: ([Int], [Int]) -> [Int]
similarities (left, right) = [ l * length (filter (== l) right) | l <- left]

main :: IO ()
main = do
  file <- readFile inputFile
  let parsedInput = parseInput file
      sortedInput = bimap sort sort parsedInput
  putStrLn $ "Part 1: " ++ show (sum $ distances sortedInput)
  putStrLn $ "Part 2: " ++ show (sum $ similarities parsedInput)
