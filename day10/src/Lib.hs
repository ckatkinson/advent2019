module Lib
    ( ten
    ) where
    
import Data.Set (Set)
import qualified Data.Set as S


data Obj = Asteroid | Empty deriving (Show, Eq)
type Point = (Int, Int)
type SpacePoint = (Point, Obj)

readObj :: Char -> Obj
readObj '.' = Empty
readObj '#' = Asteroid

getInput :: FilePath -> IO [SpacePoint]
getInput path = do contents <- readFile path
                   return (go contents)
  where go xs = concatMap (\ l -> 
                             (map (\ x -> 
                                     ((fst x, snd l), readObj (snd x))) (fst l)))  
                          (zip (map (zip [0..]) (lines xs)) [0..])

asteroids :: [SpacePoint] -> [Point]
asteroids sps = map fst $ filter (\ x -> snd x == Asteroid) sps

-- Manhattan is fine
distance :: Point -> Point -> Int
distance (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

pointsOnRay :: Point -> Point -> [Point] -> Set Point
pointsOnRay (x1,y1) (x2,y2) pts = S.fromList $ filter check pts
  where check (x,y) = (y - y1) * (x2 - x1) == (y2 - y1) * (x - x1) &&
                      (x - x1) * (x2 - x1) + (y - y1) * (y2 - y1) > 0

raysAtPoint :: Point -> [Point] -> Set (Set Point)
raysAtPoint p pts = S.fromList $ 
                     map (\ x -> pointsOnRay p x pts) (filter (/=p) pts)
-- raysAtPoint p pts = S.filter (\x -> S.size x > 1) $
                     -- S.fromList $
                     -- map (\ x -> pointsOnRay p x pts) (filter (/=p) pts)

asteroidOnLine :: Point -> Point -> [Point] -> Bool
asteroidOnLine p1 p2 pts = length (pointsOnRay p1 p2 pts) - 1 > 0

numAsteroidsSeen :: Point -> [Point] -> Int
numAsteroidsSeen base pts = S.size (raysAtPoint base pts)

answer1 :: [Point] -> Int
answer1 pts = maximum $ map (`numAsteroidsSeen` pts) pts


ten :: IO ()
ten = do xs <- getInput "./input"
         let as = asteroids xs
         putStrLn "Part 1:"
         print $ answer1 as
        



