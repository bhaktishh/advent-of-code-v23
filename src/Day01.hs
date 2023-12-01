module Day01
    ( part1, part2
    ) where
import Data.Char
import Data.List.Split

validList = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

part1 :: String -> Int
part1 str = sum $ map (\ x -> read $ (head (filter isDigit x)) : [(last (filter isDigit x))]) (lines str)

part2 :: String -> Int
part2 str = sum $ map (\x -> (10 * part2Helper x) + (part2Helper2 (reverse x))) $ lines str

part2Helper :: String -> Int
part2Helper chars@(x:xs) = if isDigit x then (digitToInt x) else
    case (take 3 chars) of 
    "one" -> 1
    "two" -> 2
    "six" -> 6
    _ -> case (take 4 chars) of 
        "four" -> 4
        "five" -> 5
        "nine" -> 9
        _ ->  case (take 5 chars) of 
           "three" -> 3
           "seven" -> 7
           "eight" -> 8
           _ -> part2Helper xs

part2Helper2 :: String -> Int
part2Helper2 chars@(x:xs) = if isDigit x then (digitToInt x) else
    case (take 3 chars) of 
   "eno" -> 1
   "owt" -> 2
   "xis" -> 6
   _ -> case (take 4 chars) of 
       "ruof" -> 4
       "evif" -> 5
       "enin" -> 9
       _ ->  case (take 5 chars) of 
           "eerht" -> 3
           "neves" -> 7
           "thgie" -> 8
           _ -> part2Helper2 xs