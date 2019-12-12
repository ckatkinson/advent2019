module Lib
    ( twelve
    ) where

import Data.Set (Set)
import qualified Data.Set as S

input :: [Position]
input =  [(7 , 10, 17),
          (-2, 7 , 0),
          (12, 5 , 12),
          (5 , -8, 6)]

testinput =  [(-1, 0, 2),
              (2, -10, -7),
              (4, -8, 8),
              (3, 5, -1)]

initSystem :: System
initSystem = map (\ p -> Moon p (0,0,0)) input

testinitSystem :: System
testinitSystem = map (\ p -> Moon p (0,0,0)) testinput

m1 = head testinitSystem
m2 = testinitSystem !! 1
m3 = testinitSystem !! 2
m4 = testinitSystem !! 3


type Position = (Int, Int, Int)
type Velocity = (Int, Int, Int)
data Moon     = Moon { position :: Position
                     , velocity :: Velocity } deriving (Show, Eq, Ord)
type System   = [Moon]

-- Utility

addV :: Position -> Position -> Position
addV (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2) 

xcoord :: (Int, Int, Int) -> Int
xcoord (x, _, _) = x

ycoord :: (Int, Int, Int) -> Int
ycoord (_, y, _) = y

zcoord :: (Int, Int, Int) -> Int
zcoord (_, _, z) = z

--

-- Dynamics

compGravChange :: Int -> Int -> Int
compGravChange x1 x2 
 | x1 > x2   = -1
 | x1 < x2   = 1
 | otherwise = 0

-- Careful: This is non-commutative. This is computing the new velocity for the
-- first argument.
velGravChange :: Moon -> Moon -> Velocity
velGravChange moon1 moon2 = dVel
  where newVelocity  = velocity moon1 `addV` dVel
        (x1, y1, z1) = position moon1
        (x2, y2, z2) = position moon2
        dVel         = (compGravChange x1 x2, compGravChange y1 y2, compGravChange z1 z2)


applyGravityMoon :: System -> Moon -> Velocity
applyGravityMoon sys moon = foldl addV (velocity moon) (map (velGravChange moon) sys)

stepMoon :: System -> Moon -> Moon
stepMoon sys moon = Moon newPos newVel
  where newVel = applyGravityMoon sys moon
        vel    = velocity moon
        newPos = position moon `addV` newVel

stepSystem :: System -> System
stepSystem sys = map (stepMoon sys) sys
--

-- Energy

potential :: Moon -> Int
potential moon = abs (xcoord pos) + abs (ycoord pos) + abs (zcoord pos)
  where pos = position moon

kinetic :: Moon -> Int
kinetic moon = abs (xcoord vel) + abs (ycoord vel) + abs (zcoord vel)
  where vel = velocity moon

total :: Moon -> Int
total moon = potential moon * kinetic moon

energy :: System -> Int
energy sys = sum $ map total sys

-- 

answer1 :: System -> Int -> Int
answer1 sys n = energy $ iterate stepSystem sys !! n

-- Part 2

stepsToRepeat :: System -> Set System -> Int
stepsToRepeat sys seen 
  | S.notMember sys seen = stepsToRepeat (stepSystem sys) newSeen
  | otherwise            = S.size seen
  where newSeen = S.insert sys seen

-- Probably too slow, but let's try:
answer2 :: System -> Int
answer2 sys = stepsToRepeat sys S.empty

twelve :: IO ()
twelve = do putStrLn "Part 1:"
            print $ answer1 initSystem 1000
            putStrLn "Part 2:"
            print $ answer2 initSystem
