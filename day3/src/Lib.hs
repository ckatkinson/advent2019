{-# OPTIONS_GHC -Wall #-}

module Lib
    ( three
    ) where

import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M
import Data.List.Split (splitOneOf)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S

type Wire = [String]
                   
getLines :: FilePath -> IO [Wire]
getLines pth = do contents <- readFile pth
                  return $ map (splitOneOf ", ") $ lines contents

data Point a = Point 
                  { xcoord :: a
                  , ycoord :: a
                  } deriving (Show, Eq, Ord)

type Vector = Point Int


data Direction = U | D | L | R deriving (Show, Eq)

movement :: String -> Vector
movement (dir:dist) 
  | dir == 'R'  = Point (read dist) 0
  | dir == 'L'  = Point (-1 * read dist) 0
  | dir == 'U'  = Point 0 (read dist)
  | dir == 'D'  = Point 0 (-1 * read dist)
  | otherwise         = error "Malformed input"
movement _ = Point 0 0

direction :: Vector -> Direction
direction v 
  | xcoord v > 0 = R
  | xcoord v < 0 = L
  | ycoord v > 0 = U
  | ycoord v < 0 = D
  | otherwise    = error "Malformed vector"


interval :: Point Int -> Vector -> [Point Int]
interval pt vect
  | direction vect == R =  map (\p -> Point (xcoord pt + p) (ycoord pt))
                             [0 .. xcoord vect]
  | direction vect == L =  map (\p -> Point (xcoord pt - p) (ycoord pt))
                             [0 .. abs (xcoord vect)]
  | direction vect == U =  map (\p -> Point (xcoord pt) (ycoord pt + p))
                             [0 .. ycoord vect]
  | direction vect == D =  map (\p -> Point (xcoord pt) (ycoord pt - p))
                             [0 .. abs (ycoord vect)]
  | otherwise           = error "Vector is invalid"

addPoints :: Point Int -> Point Int -> Point Int
addPoints p1 p2 = Point (xcoord p1 + xcoord p2) (ycoord p1 + ycoord p2)

path :: Wire -> Point Int -> Set (Point Int)
path [] _            = S.empty
path (inst:insts) pt = S.union int (path insts (addPoints pt vect))
  where vect = movement inst
        int  = S.fromList $ interval pt vect

origin :: Point Int
origin = Point 0 0

pairIntersect :: Wire -> Wire -> Set (Point Int)
pairIntersect i1 i2 = S.intersection (path i1 origin) (path i2 origin)

allIntersections :: [Wire] -> Set (Point Int)
allIntersections ins =   S.filter (/= origin) $
                         head $
                         map (uncurry pairIntersect) 
                             [(i,j) | i <- ins, j <- ins, i/=j]

manDistance :: Point Int -> Int
manDistance p = abs (xcoord p) + abs (ycoord p)

answer1 :: [Wire] -> Int
answer1 ins = fromMaybe 0 $ S.lookupMin $ S.map manDistance (allIntersections ins)


-- PART 2. Sets aren't going to work out, so have to redo some stuff.

type WirePath = Map (Point Int) Int

buildPath :: Wire ->      -- ^ wire
             Point Int -> -- ^ initial point
             Int ->       -- ^ initial index
             WirePath
buildPath [] _ _    = M.empty
buildPath (w:ws) pt ix = M.union (M.fromList $ zip inter [ix ..])
                                 (buildPath ws (addPoints pt v) (ix + ixs) )
  where v = movement w
        inter = init $ interval pt v
        ixs = length inter

stepsAtIntersect :: Wire -> Wire -> [( Int,  Int)]
stepsAtIntersect w1 w2 = [( fromMaybe 0 $ wp1 !? p, fromMaybe 0 $ wp2 !? p) | p <- S.toList $ pairIntersect w1 w2 ]
  where wp1 = buildPath w1 origin 0
        wp2 = buildPath w2 origin 0

answer2 :: [Wire] -> Int
answer2 ws = minimum $ filter (/=0) $
                       map (uncurry (+)) $ 
                       concatMap (uncurry stepsAtIntersect)
                       [ (i, j) | i <- ws, j <- ws, i/=j ]

three :: IO ()
three = do xs <- getLines "./input"
           putStrLn "The answer to part 1: "
           print $ answer1 xs
           putStrLn "The answer to part 2: "
           print $ answer2 xs
