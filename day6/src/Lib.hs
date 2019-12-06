{-# OPTIONS_GHC -Wall #-}
module Lib
    ( six
    ) where

import Data.Function
import Data.List (maximumBy, intersect)
import Data.List.Split (splitOneOf)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S

type Planet = String
type OrbitMap = [(Planet, Planet)]

getInput :: FilePath -> IO OrbitMap
getInput path = do contents <- readFile path
                   let lpairs = map (splitOneOf ")") $ lines contents
                   return $ map (\ [x,y] -> (x,y) ) lpairs
                   

planets :: OrbitMap -> Set Planet
planets op = S.union (S.fromList $ map fst op) (S.fromList $ map snd op)

-- (sane people call this parent)
coSatellite :: Planet -> OrbitMap -> Maybe Planet
coSatellite p ops = 
  if null ps
    then Nothing
    else Just $ fst $ head ps 
  where ps = filter (\ x -> snd x == p) ops

coSatellites :: Planet -> OrbitMap -> [Maybe Planet]
coSatellites p om
  | isNothing coS = []
  | otherwise     = coS : coSatellites (fromJust coS) om 
    where coS = coSatellite p om

nearestCommon :: Planet -> Planet -> OrbitMap -> Maybe Planet
nearestCommon p1 p2 om = maximumBy (compare `on` depth) commonSats
  where depth x     = planetDepth (fromMaybe "COM" x) om 
        commonSats = coSatellites p1 om `intersect` coSatellites p2 om 

planetDepth :: Planet -> OrbitMap -> Int
planetDepth p om
  | isNothing cS = 0
  | otherwise    = 1 + planetDepth (fromJust cS) om
  where cS = coSatellite p om

answer1 :: OrbitMap -> Int
answer1 om = sum [ planetDepth p om | p <- S.toList $ planets om ]

-- p1 and p2 should be coSat related
commonDistance :: Planet -> Planet -> OrbitMap -> Int
commonDistance p1 p2 om = abs $ planetDepth p1 om - planetDepth p2 om

answer2 :: OrbitMap -> Int
answer2 om = commonDistance "YOU" transfer om +
             commonDistance "SAN" transfer om - 2
  where transfer = fromJust $ nearestCommon "SAN" "YOU" om

six :: IO ()
six = do om <- getInput "./input"
         putStrLn "Part 1:"
         print $ answer1 om
         putStrLn "Part 2:"
         print $ answer2 om
