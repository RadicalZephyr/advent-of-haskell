module Main where

import qualified Data.Set as Set

main :: IO ()
main = do
  input <- getContents
  let all_lines = lines input
      frequencies = map readInt all_lines
      part1_answer = show $ part1 frequencies
      part2_answer = show $ part2 $ cycle frequencies
    in
    putStrLn $ "Part 1: " ++ part1_answer ++ "\nPart 2: " ++ part2_answer

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Maybe Int
part2 frequencies =
  iter Set.empty 0 frequencies
  where iter seen current frequencies
          | Set.member current seen = Just current
          | otherwise = case frequencies of
                          [] -> Nothing
                          (h:rest) -> iter (Set.insert current seen) (current + h) rest

readInt :: String -> Int
readInt [] = error "Can't read an empty string"
readInt whole@(first:rest)
  | first == '+' = read rest
  | otherwise = read whole
