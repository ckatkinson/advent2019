-- {-# OPTIONS_GHC -Wall #-}
module Lib
    ( eight
    ) where

import Data.Char (digitToInt)
import Data.Function
import Data.List
import Data.List.Split 

getInput :: FilePath -> IO [Int]
getInput path = do contents <- readFile path
                   return (map digitToInt $ init contents)

type Image = [Int]
type Layers = [[Int]]
type Width = Int
type Height = Int

getLayers :: Width -> Height -> Image -> Layers
getLayers w h im = chunksOf (w * h) im

count :: Int -> Image -> Int
count n im = length $ filter (== n) im

answer1 :: Image -> Int
answer1 im = ones * twos
  where layers = getLayers 25 6 im
        mlayer = minimumBy (compare `on` count 0) layers
        ones = count 1 mlayer
        twos = count 2 mlayer

makeImage :: Layers -> Image
makeImage ls = map (head . dropWhile (==2)) $ transpose ls

makePbm :: Width -> Height -> Image -> IO ()
makePbm w h im = writeFile "./message.pbm" pbm 
  where pbm = "P1 " ++ show w ++ " " ++ show h ++ " " ++ unwords (map show im)

answer2 :: Image -> IO ()
answer2 im = makePbm 25 6 $ makeImage $ getLayers 25 6 im


eight :: IO ()
eight = do xs <- getInput "./input"
           print $ answer1 xs
           answer2 xs
           

