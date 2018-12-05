module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  input <- getContents
  let ids = lines input
      part1_answer = show $ part1 ids
      -- part2_answer = show $ part2 $ cycle frequencies
    in
    putStrLn $ "Part 1: " ++ part1_answer

part1 :: [String] -> Int
part1 ids =
  let (twos, threes) = foldl sumPairs (0,0) $ map countTwosAndThrees ids
  in
    twos * threes
  where sumPairs (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
        countTwosAndThrees = twosAndThrees . valueSet . countCharacters

twosAndThrees :: Set.Set Int -> (Int, Int)
twosAndThrees s =
  (hasValue 2 s, hasValue 3 s)
  where hasValue x = boolToInt . Set.member x
        boolToInt True = 1
        boolToInt False = 0

valueSet :: Map.Map Char Int -> Set.Set Int
valueSet m =
  Set.fromList $ map snd $ Map.toList m

countCharacters :: String -> Map.Map Char Int
countCharacters box_id =
  foldr (\x -> Map.insertWith (+) x 1) Map.empty box_id
