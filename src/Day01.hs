module Day01
    ( part1, part2
    ) where

import Data.List.Split ( splitOn )
import Data.List ( sort )

total_list :: String -> [Int]
total_list = (fmap (foldl (+) 0 . fmap read)) . (splitOn [""]) . lines 

part1 :: String -> Int
part1 = maximum . total_list

part2 :: String -> Int
part2 = sum . (take 3) . reverse . sort . total_list