{-# OPTIONS_GHC -Wall #-}
module Lib
    ( sixteen
    ) where

import Data.Char

getInput :: FilePath -> IO [Int]
getInput path = do contents <- readFile path
                   return (map digitToInt (init contents))

dotProd :: Num a => [a] -> [a] -> a
dotProd [] _ = 0
dotProd _ [] = 0
dotProd (x:xs) (y:ys) = x * y + dotProd xs ys

patter :: Int -> [Int]
patter n = drop 1 $ concatMap (replicate n) $ cycle [0,1,0,-1]

fft :: [Int] -> [Int]
fft lst = map ((`mod` 10) . abs . fft') [1 .. end]
  where fft' n = dotProd lst (patter n)
        end = length lst

answer1 :: [Int] -> String
answer1 lst =  unwords $ map show $ take 8 $ iterate fft lst !! 100

nRepeats :: Int -> [Int] -> [Int]
nRepeats n ls = take (n * length ls) $ cycle ls

vSums :: [Int] -> [Int]
vSums [] = []
vSums [x] = [x]
vSums (x:y:xs) = x : vSums ((x+y):xs) 


-- A nice problem! Note how big the offset is. It's more than half the length of
-- the input repeated 10,000 times. This means that when we do the dot product,
-- we're just seeing [0,..,0,1,..1] where the 1's start after the offset. This
-- means that we only need to sum. vSums above does this in O(n) (assuming
-- we reverse the list).

answer2 :: [Int] -> String
answer2 lst = unwords $ map show $ take 8 $ reverse $ 
              iterate (map (`mod` 10) . vSums) revBiglst !! 100
  where revBiglst = reverse $ drop (offset lst) $ nRepeats 10000 lst

offset :: [Int] -> Int
offset = listToInt . take 7

listToInt :: [Int] -> Int
listToInt ns = read $ concatMap show (take 7 ns) 

sixteen :: IO ()
sixteen = do 
  xs <- getInput "./input"
  putStrLn "Part 1:"
  print $ answer1 xs
  putStrLn "Part 2:"
  print $ answer2 xs
        
