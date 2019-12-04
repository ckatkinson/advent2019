{-# OPTIONS_GHC -Wall #-}
module Main where

getLines :: FilePath -> IO [Int]
getLines path = do contents <- readFile path
                   return (map read $ lines contents)

answer1 :: [Int] -> Int
answer1 ss = sum $
             map (\x -> (x `div` 3) - 2) ss

fuelCalc :: Int -> Int
fuelCalc f
  | op <= 0   = 0
  | otherwise = op + fuelCalc op
  where op = f `div` 3 - 2

answer2 :: [Int] -> Int
answer2 ss = sum $
             map fuelCalc ss

main :: IO ()
main = do xs <- getLines "./input"
          putStrLn "Answer 1:"
          print $ answer1 xs
          putStrLn "Answer 2:"
          print $ answer2 xs
