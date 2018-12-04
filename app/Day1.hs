module Main where

import Lib

main = do
  input <- getContents
  let frequencies = lines input in
    putStrLn $ show $ sum $ map readInt $ frequencies

part1 lines =
  sum

readInt :: String -> Int
readInt whole@(head:tail)
  | head == '+' = read tail
  | otherwise = read whole
