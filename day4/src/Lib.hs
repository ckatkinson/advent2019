module Lib
    ( four
    ) where

import Data.List (group, sort, nub)

range :: [Int]
range = [235741 .. 706948]

repeatedDigit :: Int -> Bool
repeatedDigit n = length (nub $ show n) < length (show n)

increasingDigits :: Int -> Bool
increasingDigits n = sort (show n) == show n

answer1 :: [Int] -> Int
answer1 ns = length $ filter (\x -> repeatedDigit x && increasingDigits x)
                      range

atLeastOnePair :: Int -> Bool
atLeastOnePair n = any (\x -> length x == 2) $ group $ show n

answer2 :: [Int] -> Int
answer2 ns = length $ filter (\x -> atLeastOnePair x && increasingDigits x)
                      range

four:: IO ()
four = do putStrLn "Answer to part 1:"
          print $ answer1 range
          putStrLn "Answer to part 2:"
          print $ answer2 range
