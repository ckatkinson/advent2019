{-# OPTIONS_GHC -Wall #-}
module Lib
    ( ten
    ) where
    
import Data.Function
import Data.List
import Data.Set (Set)
import qualified Data.Set as S


data Obj = Asteroid | Empty deriving (Show, Eq)
type Point = (Int, Int)
type SpacePoint = (Point, Obj)
type Ray = Set Point

readObj :: Char -> Obj
readObj '.' = Empty
readObj '#' = Asteroid
readObj _   = error "Check yo input!"

getInput :: FilePath -> IO [SpacePoint]
getInput path = do contents <- readFile path
                   return (go contents)
  where go xs = concatMap (\ l -> 
                             (map (\ x -> 
                                     ((fst x, snd l), readObj (snd x))) (fst l)))  
                          (zip (map (zip [0..]) (lines xs)) [0..])

asteroids :: [SpacePoint] -> [Point]
asteroids sps = map fst $ filter (\ x -> snd x == Asteroid) sps


pointsOnRay :: Point -> Point -> [Point] -> Ray
pointsOnRay (x1,y1) (x2,y2) pts = S.fromList $ filter check pts
  where check (x,y) = (y - y1) * (x2 - x1) == (y2 - y1) * (x - x1) &&
                      (x - x1) * (x2 - x1) + (y - y1) * (y2 - y1) > 0

raysAtPoint :: Point -> [Point] -> Set Ray
raysAtPoint p pts = S.fromList $ 
                     map (\ x -> pointsOnRay p x pts) (filter (/=p) pts)

numAsteroidsSeen :: Point -> [Point] -> Int
numAsteroidsSeen base pts = S.size (raysAtPoint base pts)

answer1 :: [Point] -> Int
answer1 pts = maximum $ map (`numAsteroidsSeen` pts) pts

-- Laser base location
baseLoc :: [Point] -> Point
baseLoc pts = maximumBy (compare `on` (`numAsteroidsSeen` pts)) pts

data Direction = Up | FstQ Float | Rght | 
                 SndQ Float | Down | ThrdQ Float | 
                 Lft | FrthQ Float deriving (Show, Eq, Ord)

rayDirection :: Point -> Ray -> Direction
rayDirection (b1,b2) ray 
  | upward       = Up
  | downward     = Down
  | left         = Lft
  | right        = Rght 
  | fQ           = FstQ slope
  | sQ           = SndQ slope
  | tQ           = ThrdQ slope
  | lQ           = FrthQ slope
  | otherwise    = error "A ray must contain at least one point to have a direction from base"
    where (p1,p2)    = S.elemAt 0 ray
          upward     = b1 == p1 && p2 < b2
          downward   = b1 == p1 && p2 > b2
          left       = b2 == p2 && p1 < b1
          right      = b2 == p2 && p1 > b1
          fi         = fromIntegral
          slope      =  (fi p2 - fi b2) / (fi p1 - fi b1)
          fQ         = p1 > b1 && p2 < b2
          sQ         = p1 > b1 && p2 > b2
          tQ         = p1 < b1 && p2 > b2
          lQ         = p1 < b1 && p2 < b2

sortedRays :: Point -> Set Ray -> [Ray]
sortedRays base rays = sortBy (compare `on` rayDirection base) (S.toList rays)

-- There are more than 200 rays, so we can just look at the 200th entry. Luckily
-- there is no wraparound, so we don't even need to vaporize any asteroids for a
-- second lap!
answer2 :: [Point] -> Ray
answer2 pts = sortedRays base rays !! 199
  where base = baseLoc pts
        rays = raysAtPoint base pts

ten :: IO ()
ten = do xs <- getInput "./input"
         let as = asteroids xs
         putStrLn "Part 1:"
         print $ answer1 as
         putStrLn "Part 2:"
         print $ answer2 as
        



